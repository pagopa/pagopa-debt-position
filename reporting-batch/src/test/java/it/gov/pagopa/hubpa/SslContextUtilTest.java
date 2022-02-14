package it.gov.pagopa.hubpa;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import it.gov.pagopa.hubpa.utils.SslContextUtil;

import javax.net.ssl.SSLSocketFactory;

@ExtendWith(MockitoExtension.class)
class SslContextUtilTest {

    @Test
    void runCheckSSLContextTest() throws Exception {
        String certPagoPa = "-----BEGIN CERTIFICATE-----\n"
                + "MIIDbjCCAlYCCQCqCOJPdeVj6DANBgkqhkiG9w0BAQsFADB5MQswCQYDVQQGEwJJ\n"
                + "VDENMAsGA1UECAwEUm9tYTENMAsGA1UEBwwEUm9tYTEPMA0GA1UECgwGUGFwYXBh\n"
                + "MQwwCgYDVQQLDANkZXYxEDAOBgNVBAMMB3Rlc3QuaXQxGzAZBgkqhkiG9w0BCQEW\n"
                + "DHRlc3RAdGVzdC5pdDAeFw0yMTA3MTUxMDM3NTBaFw0yMjA3MTUxMDM3NTBaMHkx\n"
                + "CzAJBgNVBAYTAklUMQ0wCwYDVQQIDARSb21hMQ0wCwYDVQQHDARSb21hMQ8wDQYD\n"
                + "VQQKDAZQYXBhcGExDDAKBgNVBAsMA2RldjEQMA4GA1UEAwwHdGVzdC5pdDEbMBkG\n"
                + "CSqGSIb3DQEJARYMdGVzdEB0ZXN0Lml0MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8A\n"
                + "MIIBCgKCAQEAxvXkTvINAzjm7r+844AeB/GkP3JYQZkF21Gi6H21eZez2yvIawXu\n"
                + "txVGPDBetE2pGOSofvKOe+4pMN5hIyO41s/tkEOgaYofIOF/lACxsZA7xMEUB5Wz\n"
                + "USj9DUQXUSc/G6sePZK7mNQWN4sYwUVWe9BtHv9nQBHrkssZcbVANeJ8GR+l9CBB\n"
                + "C3ZMi1CFfvOXvwjaQrQDDOU9QtSsuvbidZOlqqBzaD786viJV3phOeY7bbRGYtT8\n"
                + "s3qP31V/IqMa1m+CsAzWD2xUD3yNlVhADoXNbOQghOqceD/7cV4hYK1UCU3qE2ID\n"
                + "ZYykY9DNX39ERqbi6Wgn87Y03hcBDRNi1QIDAQABMA0GCSqGSIb3DQEBCwUAA4IB\n"
                + "AQBHWKXl8YkqEhXaGDr/ZO5GeCnll+4UsXke2Cr/+CjCxVdPUbGs/Q0hDDy7qJ3j\n"
                + "wynAeEInDJJw9iDZEuhrP3KGlObv6zdAYkhD7Uf9vkUn1wnJ9Hd67hkis5I2iNJb\n"
                + "FXi25bJ/m2iJj3QARnOdV0mv014P8Pvb63t/S77Go7M0hPpJg7HFdTvU0JrE0h5u\n"
                + "H1TGOFO+6wfK5G+oIOpEJWainMX8+IAUHRG2F0Ym4pTbQbrAPHzxBIleJ8hQAIU/\n"
                + "Ml1DQnRI5lsSnC01TO7TIuqYZCutqdoOYp9Wx+Jf+mNIzpzWLEK+/mX9nhAIt+eB\n" + "Z6Sd7YsDC0eqekaXGs0s4ojs\n"
                + "-----END CERTIFICATE-----";

        String key = "-----BEGIN PRIVATE KEY-----\n"
                + "MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQDG9eRO8g0DOObu\n"
                + "v7zjgB4H8aQ/clhBmQXbUaLofbV5l7PbK8hrBe63FUY8MF60TakY5Kh+8o577ikw\n"
                + "3mEjI7jWz+2QQ6Bpih8g4X+UALGxkDvEwRQHlbNRKP0NRBdRJz8bqx49kruY1BY3\n"
                + "ixjBRVZ70G0e/2dAEeuSyxlxtUA14nwZH6X0IEELdkyLUIV+85e/CNpCtAMM5T1C\n"
                + "1Ky69uJ1k6WqoHNoPvzq+IlXemE55jtttEZi1Pyzeo/fVX8ioxrWb4KwDNYPbFQP\n"
                + "fI2VWEAOhc1s5CCE6px4P/txXiFgrVQJTeoTYgNljKRj0M1ff0RGpuLpaCfztjTe\n"
                + "FwENE2LVAgMBAAECggEAGZhigaFz+Rkl5Er4UtAVjPISLjNYlT/JWBide4lIglb4\n"
                + "xVZRlysRNa5f9bhHRqzC9zfbUVGE8P6HgAjruCiNS90985Qvm8QwEvvPfMvGEFC8\n"
                + "z6gDGqKwid1bCIzc7wy8eqO95S/uQE/wd77GNX7lDFKY5yb2MnDbvGuyX0Vw+D1r\n"
                + "cBMinrXh56ptdqRNNcmc7LxgmhX8FR0QpwzWSXPwC7C+8YaA5/n7JjvKzbXkDjss\n"
                + "dq9RMXWLxj71kccNuos6u+p13mQIHjqOxEpzBtc6uqHciU7m9E/YGFiqoXMS8q2P\n"
                + "zcNIoSyl2ZrRWhbkrLv9f1TLJnZElX4UJqkvy0+UwQKBgQD1zl6hJNv52l/ZlRfg\n"
                + "OawaHFIk/k80EFMdEl1TO9UUUzYcZejv+h1A4pKXv9Y3oLTR04vfplULnw2dA/Ra\n"
                + "T9hf3XpVY6B6mL4T2GK0z3bB925LZRUf/kQcKnDokaB/SDAssC9CKNEg+nKkQwqq\n"
                + "a6nhnDRvJUwe8XsnmyjF8sg0xQKBgQDPNi4YlNSzmvzZIo9eu2au93svIn1WcCHY\n"
                + "pd6T0TrXKebqJvJJBoCeXEvMkC5TjtT2hsME6O4v0YLYyDssXa3BKZCWTV4Z3oFn\n"
                + "NRcifP/HVSsl/d3ARNuBLcBolqIFJqdI1HTJfC8UsZY4swAMT+hi5iJWRlQdyPli\n"
                + "j+x8fe/20QKBgDdzFmXDqtvyJy0uNPSgDfLV8LHnrHZSOG2WdvcPyEGNg+dkFegM\n"
                + "xAyfD/Krk+d4mVG8JxGMtCKq/qcs/TJiUK1PiQk6MDl3u2k+pwWix2d6KadYZiuR\n"
                + "cEvqRw4vy8Tqw/NQy1hhXMvTs4jals1a/DeoxStNfp58WwvUNJ5y5jcBAoGBAL+4\n"
                + "48G+w2dDOjw16+28+29ccM/V256Etexc3KpsZ0L59DwmuPq0V4Eu6LtnlFWfzJAl\n"
                + "dIAwfWIlOioWahnMu54ENVG8WBsbcyPpTXSNr6Phu8C1Od5SV78Yc/TRmyuk7hdG\n"
                + "7KAYlP9SqSnhBWRe8ye+w3qMK/w7HfQCMs+lPshhAoGATJGoitcEaffjoJMipNtx\n"
                + "9QzPIf9jfj7H5a5+frmjdYEOH4EcQ0az0P2apj0twUD3Yu2BOc4YCnfzEOr/5Hz9\n"
                + "t5OKJ2UEwwNzzqRaz9E7gnONyT09pb0rI3u2IYwzZt/uQKLGn+f6C3t77J37kINo\n" + "gN4c44m6wDv/oFdcUkESrUQ=\n"
                + "-----END PRIVATE KEY-----";

        SSLSocketFactory sslSocketFactory = SslContextUtil.getSslContext(certPagoPa, key, null);

        assertNotNull(sslSocketFactory);
    }

}
