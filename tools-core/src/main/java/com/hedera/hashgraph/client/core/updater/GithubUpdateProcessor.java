package com.hedera.hashgraph.client.core.updater;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.security.SecureRandom;
import java.security.SignatureException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.hedera.hashgraph.client.core.remote.SoftwareUpdateFile;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.http.client.HttpResponseException;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.conn.ssl.NoopHostnameVerifier;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.maven.artifact.versioning.ComparableVersion;

import static com.hedera.hashgraph.client.core.constants.Constants.GPG_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.SOFTWARE_UPDATE_EXTENSION;
import static com.hedera.hashgraph.client.core.constants.Constants.TXT_EXTENSION;
import static com.hedera.hashgraph.client.core.remote.SoftwareUpdateFile.getSoftwareVersionFromVersionStr;
import static com.hedera.hashgraph.client.core.security.SecurityUtilities.verifySignature;

public class GithubUpdateProcessor {

	private static final Logger logger = LogManager.getLogger(GithubUpdateProcessor.class);

    private static final String CHECK_TAG = "ReleaseToAll";

    private final AtomicBoolean keepRunning;
    private final String curVersion;
    private final File outputDir;

    boolean wasOutputModified;

    public GithubUpdateProcessor(AtomicBoolean keepRunning, final String curVersion, final File outputDir) {
        this.keepRunning = keepRunning;
        this.curVersion = curVersion;
        this.outputDir = outputDir;
    }

    boolean doCheck() throws Exception {

        if (!outputDir.exists()) {
            return false;
        }

        var updates = getUpdates();

        if (!keepRunning.get()) {
            return false;
        }

        if (!cleanupOutputDir(updates)) {
            return false;
        }

        return downloadAndVerifyUpdates(updates);
    }

    private boolean downloadAndVerifyUpdates(Map<String, UpdateFile> updates) throws IOException {
        var curVersionParsed = new ComparableVersion(getSoftwareVersionFromVersionStr(curVersion));

        File tmpDir = null;

        try {

            var sorted = new TreeMap<ComparableVersion, UpdateFile>();

            for (var update : updates.values()) {
                var key = new ComparableVersion(update.version);
                if (sorted.containsKey(key)) {
                    logger.error("version {} is in multiple releases!", update.version);
                } else {
                    sorted.put(key, update);
                }
            }

            ArrayList<String> downloaded = new ArrayList<>();

            boolean downloadedOne = false;

            for (var entry : sorted.descendingMap().entrySet()) {

                var update = entry.getValue();

                if (curVersionParsed.compareTo(entry.getKey()) >= 0) {
                    logger.debug("ignoring update {}, it is for an older version {}, curVersion {}", update.name,
                            update.version, curVersionParsed.getCanonical());
                    continue;
                }

                //download 3 latest versions
                if (downloaded.size() >= 3) {
                    (downloadedOne ? logger.atInfo() : logger.atDebug())
                            .log("The 3 newest versions are downloaded, not downloading more - {}", downloaded);
                    break;
                }

                File updateFileFinal = new File(outputDir, update.name);

                if (update.exists) {
                    logger.debug("ignoring update {}, it already exists in {}", update.name, outputDir.getAbsolutePath());
                    downloaded.add(updateFileFinal.getAbsolutePath());
                    continue;
                }

                if (tmpDir == null) {
                    tmpDir = Files.createTempDirectory("GithubUpdateProcessor").toFile();
                }

                File updateFile = new File(tmpDir, update.name);
                File sigFile = getSigFile(updateFile);
                File commentFile = getCommentFile(updateFile);

                File sigFileFinal = getSigFile(updateFileFinal);
                File commentFileFinal = getCommentFile(updateFileFinal);


                try {

                    if (updateFileFinal.exists()) {
                        logger.warn("update {} exists, overwriting it", updateFileFinal.getAbsolutePath());
                        wasOutputModified = true;
                        if (!updateFileFinal.delete()) {
                            throw new IOException("unable to delete " + updateFileFinal.getAbsolutePath());
                        }
                    }

                    if (sigFileFinal.exists()) {
                        logger.warn("update sig {} exists, overwriting it", sigFileFinal.getAbsolutePath());
                        wasOutputModified = true;
                        if (!sigFileFinal.delete()) {
                            throw new IOException("unable to delete " + sigFileFinal.getAbsolutePath());
                        }
                    }

                    if (commentFileFinal.exists()) {
                        logger.warn("update comment {} exists, overwriting it", commentFileFinal.getAbsolutePath());
                        wasOutputModified = true;
                        if (!commentFileFinal.delete()) {
                            throw new IOException("unable to delete " + commentFileFinal.getAbsolutePath());
                        }
                    }

                    logger.info("downloading update file {} to {}", update.downloadUrl, updateFile.getAbsolutePath());
                    downloadFile(update.downloadUrl, updateFile);

                    logger.info("downloading sig file {} to {}", update.sigDownloadUrl, sigFile.getAbsolutePath());
                    downloadFile(update.sigDownloadUrl, sigFile);

                    logger.info("writing comment file {}", commentFile.getAbsolutePath());
                    FileUtils.write(commentFile, update.getNotesJson(), "UTF8");

                    if (!verifySignature(updateFile.getAbsolutePath())) {
                        logger.warn("downloaded signature is invalid for update {}", sigFile.getAbsolutePath());
                        throw new SignatureException();
                    }

                    wasOutputModified = true;

                    logger.info("moving update file {} to {}", updateFile.getAbsolutePath(), updateFileFinal.getAbsolutePath());
                    FileUtils.moveFile(updateFile, updateFileFinal);

                    logger.info("moving update file sig {} to {}", updateFile.getAbsolutePath(), updateFileFinal.getAbsolutePath());
                    FileUtils.moveFile(sigFile, sigFileFinal);

                    logger.info("moving update file comment {} to {}", updateFile.getAbsolutePath(), updateFileFinal.getAbsolutePath());
                    FileUtils.moveFile(commentFile, commentFileFinal);

                    if (!verifySignature(updateFileFinal.getAbsolutePath())) {
                        logger.error("downloaded signature is invalid after move {}", sigFileFinal.getAbsolutePath());
                        throw new SignatureException();
                    }

                    downloadedOne = true;
                    downloaded.add(updateFileFinal.getAbsolutePath());

                } catch (Exception ex) {
                    logger.error("failed to download and verify update " + update.name + " " + update.downloadUrl, ex);

                    if (updateFile.exists() && !updateFile.delete()) {
                        logger.warn("could not delete invalid update {}", updateFile.getAbsolutePath());
                    }
                    if (sigFile.exists() && !sigFile.delete()) {
                        logger.warn("could not delete invalid update sig {}", sigFile.getAbsolutePath());
                    }
                    if (commentFile.exists() && !commentFile.delete()) {
                        logger.warn("could not delete invalid update comment {}", commentFile.getAbsolutePath());
                    }
                    if (updateFileFinal.exists()) {
                        wasOutputModified = true;
                        if (!updateFileFinal.delete()) {
                            logger.warn("could not delete invalid final update {}", updateFileFinal.getAbsolutePath());
                        }
                    }
                    if (sigFileFinal.exists()) {
                        wasOutputModified = true;
                        if (!sigFileFinal.delete()) {
                            logger.warn("could not delete invalid final update {}", sigFileFinal.getAbsolutePath());
                        }
                    }
                    if (commentFileFinal.exists()) {
                        wasOutputModified = true;
                        if (!commentFileFinal.delete()) {
                            logger.warn("could not delete invalid final update {}", commentFileFinal.getAbsolutePath());
                        }
                    }
                }


                if (!keepRunning.get()) {
                    return false;
                }
            }
        } finally {
            FileUtils.deleteQuietly(tmpDir);
        }

        return true;
    }

    private boolean cleanupOutputDir(Map<String, UpdateFile> updates) {
        if (!outputDir.exists()) {
            logger.error("output dir {} does not exist", outputDir.getAbsolutePath());
            return false;
        }

        var files = outputDir.listFiles();

        if (files == null) {
            logger.error("output dir {} is not a directory", outputDir.getAbsolutePath());
            return false;
        }

        for (var file : files) {

            Runnable doDelete = () -> {
                wasOutputModified = true;
                if (!file.delete()) {
                    logger.error("unable to delete update {}", file.getAbsolutePath());
                }
                var sigFile = getSigFile(file);
                if (sigFile.exists()) {
                    if (!sigFile.delete()) {
                        logger.error("unable to delete sig file {}", sigFile.getAbsolutePath());
                    }
                }
                var commentFile = getCommentFile(file);
                if (commentFile.exists()) {
                    if (!commentFile.delete()) {
                        logger.error("unable to delete comment file {}", commentFile.getAbsolutePath());
                    }
                }
            };

            if (SOFTWARE_UPDATE_EXTENSION.equals(FilenameUtils.getExtension(file.getName()))) {

                var update = updates.get(file.getName());

                if (update == null) {
                    logger.warn("deleting unknown update {}", file.getAbsolutePath());
                    doDelete.run();
                    continue;
                }

                if (!verifySignature(file.getAbsolutePath())) {
                    logger.warn("deleting update with invalid signature {}", file.getAbsolutePath());
                    doDelete.run();
                    continue;
                }

                final File commentFile = getCommentFile(file);

                if (!commentFile.exists()) {
                    logger.warn("deleting update with missing comment {}", file.getAbsolutePath());
                    doDelete.run();
                    continue;
                }

                update.exists = true;
            }
        }

        return true;
    }

    private File getCommentFile(File file) {
        return new File(file.getParentFile().getAbsolutePath(), FilenameUtils.getBaseName(file.getName()) + "." + TXT_EXTENSION);
    }

    private File getSigFile(File file) {
        return new File(file.getParentFile().getAbsolutePath(), getSigFileName(file.getName()));
    }

    private String getSigFileName(String name) {
        return name + "." + GPG_EXTENSION;
    }

    private Map<String, UpdateFile> getUpdates() throws Exception {
        Map<String, UpdateFile> updates = new HashMap<>();

        for (int x = 1; x < 1000; ++x) {
            var arr = getJson(
                    "https://api.github.com/repos/hashgraph/hedera-transaction-tool-demo/releases?per_page=100&page="
                            + x).getAsJsonArray();

            if (!keepRunning.get()) {
                return updates;
            }

            if (arr == null || arr.size() <= 0) {
                break;
            }


            for (var item : arr) {
                if (item.isJsonObject()) {
                    var obj = item.getAsJsonObject();
                    if (isStringProp(obj, "tag_name") && isArrayProp(obj, "assets") && isStringProp(obj, "body")
                            && isStringProp(obj, "name")) {

                        var updateFile = new UpdateFile();

                        updateFile.description = obj.get("body").getAsString();
                        updateFile.title = obj.get("name").getAsString();

                        if (updateFile.description.contains(CHECK_TAG)) {

                            var tag = obj.get("tag_name").getAsString();
                            var tagVersion = SoftwareUpdateFile.fixVersion(tag.substring(1));

                            updateFile.version = tagVersion;

                            for (var assetEl : obj.getAsJsonArray("assets")) {
                                if (isStringProp(assetEl, "name") && isStringProp(assetEl, "browser_download_url")) {
                                    var asset = assetEl.getAsJsonObject();
                                    var assetName = asset.get("name").getAsString();
                                    if (SOFTWARE_UPDATE_EXTENSION.equals(FilenameUtils.getExtension(assetName))) {
                                        try {
                                            var version = SoftwareUpdateFile.getVersionFromFileName(assetName);
                                            if (!tagVersion.equals(version)) {
                                                logger.warn("update file version {} does not match tag version {}! ", version, tagVersion);
                                                continue;
                                            }
                                        } catch (Exception ex) {
                                            logger.error("failed to get version from " + assetName, ex);
                                        }

                                        if (updateFile.name != null) {
                                            logger.warn("more than one update file present for {} {} {}!", tag, updateFile.name, assetName);
                                        } else {
                                            updateFile.name = assetName;
                                            updateFile.downloadUrl = asset.get("browser_download_url").getAsString();
                                        }
                                    }
                                }
                            }

                            if (updateFile.name != null) {

                                for (var assetEl : obj.getAsJsonArray("assets")) {
                                    if (isStringProp(assetEl, "name") && isStringProp(assetEl, "browser_download_url")) {
                                        var asset = assetEl.getAsJsonObject();
                                        var assetName = asset.get("name").getAsString();

                                        if (getSigFileName(updateFile.name).equals(assetName)) {

                                            if (updateFile.sigName != null) {
                                                logger.warn("more than one update file sig present for {} {} {}!",
                                                        tag, updateFile.sigName, assetName);
                                            } else {
                                                updateFile.sigName = assetName;
                                                updateFile.sigDownloadUrl = asset.get("browser_download_url").getAsString();

                                                if (updates.containsKey(updateFile.name)) {
                                                    logger.error("update {} is listed in multiple releases!", updateFile.name);
                                                } else {
                                                    updates.put(updateFile.name, updateFile);
                                                }
                                            }
                                        }
                                    }
                                }

                            }


                        }
                    }
                }
            }
        }

        return updates;
    }

    private JsonElement getJson(String url) throws Exception {

        try (var httpClient = getHttpClient()) {

            var resp = httpClient.execute(new HttpGet(url));

            if (resp.getStatusLine().getStatusCode() >= 300) {
                throw new HttpResponseException(resp.getStatusLine().getStatusCode(),
                        resp.getStatusLine().getReasonPhrase());
            }

            var jsonStr = EntityUtils.toString(resp.getEntity());

            return JsonParser.parseString(jsonStr);
        }
    }

    private void downloadFile(String url, File output) throws Exception {

        try (var httpClient = getHttpClient()) {

            var resp = httpClient.execute(new HttpGet(url));

            if (resp.getStatusLine().getStatusCode() >= 300) {
                throw new HttpResponseException(resp.getStatusLine().getStatusCode(),
                        resp.getStatusLine().getReasonPhrase());
            }

            FileUtils.copyInputStreamToFile(resp.getEntity().getContent(), output);
        }
    }

    private CloseableHttpClient getHttpClient() throws Exception {

        // ignore certs, we will verify releases with signature file

        TrustManager[] trustAllCerts = new TrustManager[] { new X509TrustManager() {
            @Override
            public void checkClientTrusted(X509Certificate[] chain, String authType) {
            }
            @Override
            public void checkServerTrusted(X509Certificate[] chain, String authType) {
            }
            @Override
            public X509Certificate[] getAcceptedIssuers() {
                return null;
            }
        } };

        var ctx = SSLContext.getInstance("SSL");
        ctx.init(null, trustAllCerts, new SecureRandom());

        return HttpClients
            .custom()
            .setSSLHostnameVerifier(NoopHostnameVerifier.INSTANCE)
            .setSSLContext(ctx)
            .build();
    }

    private boolean isStringProp(JsonElement el, String name) {
        if (el.isJsonObject()) {
            var obj = el.getAsJsonObject();
            if (obj.has(name)) {
                var prop = obj.get(name);
                return prop.isJsonPrimitive() && prop.getAsJsonPrimitive().isString();
            }
        }
        return false;
    }

    private boolean isArrayProp(JsonElement el, String name) {
        if (el.isJsonObject()) {
            var obj = el.getAsJsonObject();
            if (obj.has(name)) {
                var prop = obj.get(name);
                return prop.isJsonArray();
            }
        }
        return false;
    }

    private static class UpdateFile {
        private String version;
        private String name;
        private String downloadUrl;
        private String sigName;
        private String sigDownloadUrl;
        private String title;
        private String description;

        private boolean exists;

        private String getNotesJson() {
            JsonObject commentsJson = new JsonObject();
            JsonObject commentJson = new JsonObject();
            commentsJson.add("markdownNote", commentJson);
            commentJson.addProperty("title", title);
            commentJson.addProperty("contents", description);

            return commentsJson.toString();
        }
    }
}
