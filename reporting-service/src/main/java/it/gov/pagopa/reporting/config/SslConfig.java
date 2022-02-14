package it.gov.pagopa.hubpa.config;

import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocketFactory;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.KeyFactory;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.UnrecoverableKeyException;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.PKCS8EncodedKeySpec;
import java.util.Base64;

public class SslConfig {

    private SslConfig() {

        throw new IllegalStateException();
    }

    public static SSLSocketFactory getSSLSocketFactory(final String cert, final String key, final String password)
            throws IOException, CertificateException, KeyManagementException, UnrecoverableKeyException,
            NoSuchAlgorithmException, KeyStoreException, InvalidKeySpecException {

        /**
         * Init private key
         */
        String keyCleaned = key.replaceAll("-----BEGIN (.*)-----", "").replaceAll("-----END (.*)----", "")
                .replace("\r\n", "").replace("\n", "").trim();

        PKCS8EncodedKeySpec keySpec = new PKCS8EncodedKeySpec(Base64.getDecoder().decode(keyCleaned));
        KeyFactory keyFactory = KeyFactory.getInstance("RSA");
        PrivateKey privateKey = keyFactory.generatePrivate(keySpec);

        /**
         * Load client certificate
         */
        InputStream certInputStream = new ByteArrayInputStream(cert.getBytes());

        CertificateFactory cf = CertificateFactory.getInstance("X.509");
        Certificate caCert = cf.generateCertificate(certInputStream);

        /**
         * Client key and certificates are sent to server so it can authenticate the
         * client
         */
        KeyStore clientKeyStore = KeyStore.getInstance(KeyStore.getDefaultType());
        clientKeyStore.load(null, null);
        clientKeyStore.setCertificateEntry("private-certificate", caCert);
        clientKeyStore.setKeyEntry("private-key", privateKey, password != null ? password.toCharArray() : null,
                new Certificate[] { caCert });

        KeyManagerFactory keyManagerFactory = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
        keyManagerFactory.init(clientKeyStore, null);

        /**
         * Create SSL socket factory
         */
        SSLContext context = SSLContext.getInstance("TLSv1.2");
        context.init(keyManagerFactory.getKeyManagers(), null, null);

        /**
         * Return the newly created socket factory object
         */
        return context.getSocketFactory();
    }
}
