/*
 * Copyright (c) Cloud Software Group, Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   1) Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2) Redistributions in binary form must reproduce the above
 *      copyright notice, this list of conditions and the following
 *      disclaimer in the documentation and/or other materials
 *      provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.xensource.xenapi;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.config.ConnectionConfig;
import org.apache.hc.client5.http.config.RequestConfig;
import org.apache.hc.client5.http.cookie.StandardCookieSpec;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.HttpClients;
import org.apache.hc.client5.http.impl.io.PoolingHttpClientConnectionManager;
import org.apache.hc.core5.http.ContentType;
import org.apache.hc.core5.http.io.entity.StringEntity;

import java.io.IOException;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.concurrent.TimeUnit;

/**
 * Provides a JSON-RPC v2.0 client for making remote procedure calls to xapi's backend URL.
 * <br />
 * This class enables the communication to the JSON-RPC backend. The client utilizes the HttpClient class for
 * sending HTTP POST requests with JSON payloads and the ObjectMapper class from the Jackson library for
 * serialization and deserialization of JSON data.
 * <br />
 * The client can be customised by passing it as a parameter to corresponding constructor, enabling custom
 * handling of requests.
 * <br />
 * By default, the timeout for requests is set to 10 minutes (600 seconds). The default timeout for connecting to the
 * JSON-RPC backend is set to 5 seconds.
 *
 * @see CloseableHttpClient CloseableHttpClient is used to make requests and connect to the backend
 * @see ObjectMapper ObjectMapper is used to marshall requests and responses
 */
public class JsonRpcClient {
    private static final int DEFAULT_REQUEST_TIMEOUT = 600;
    private static final int DEFAULT_CONNECTION_TIMEOUT = 5;

    protected static final int MAX_CONCURRENT_CONNECTIONS = 10;

    protected static final String JSON_BACKEND_PATH = "/jsonrpc";

    protected final CloseableHttpClient httpClient;
    protected final String jsonRpcBackendUrl;
    protected final ObjectMapper objectMapper;
    protected final int requestTimeout;

    private final RequestConfig defaultRequestConfig = RequestConfig.custom()
            .setCookieSpec(StandardCookieSpec.IGNORE)
            .build();

    /**
     * Create a JsonRpcClient with default settings.
     *
     * @param jsonRpcBackendUrl the URL of the JSON-RPC backend. Usually of the form https://&lt;address&gt;.
     * @see JsonRpcClient JsonRpcClient for more info on using this class
     */
    public JsonRpcClient(URL jsonRpcBackendUrl) {
        this(jsonRpcBackendUrl, DEFAULT_REQUEST_TIMEOUT, DEFAULT_CONNECTION_TIMEOUT);
    }

    /**
     * Create a JsonRpcClient with the option to define the request and connection timeout values.
     *
     * @param jsonRpcBackendUrl the URL of the JSON-RPC backend. Usually of the form https://&lt;address&gt;.
     * @param requestTimeout    the timeout value for requests.
     * @param connectionTimeout the timeout value for the initial connection to the host.
     * @see JsonRpcClient JsonRpcClient for more info on using this class
     */
    public JsonRpcClient(URL jsonRpcBackendUrl, int requestTimeout, int connectionTimeout) {
        var connectionConfig = ConnectionConfig
                .custom()
                .setConnectTimeout(connectionTimeout, TimeUnit.SECONDS)
                .build();

        var connectionManager = new PoolingHttpClientConnectionManager();
        connectionManager.setDefaultConnectionConfig(connectionConfig);
        connectionManager.setMaxTotal(MAX_CONCURRENT_CONNECTIONS);

        this.httpClient = HttpClients
                .custom()
                .setConnectionManager(connectionManager)
                .build();
        this.jsonRpcBackendUrl = formatBackendUrl(jsonRpcBackendUrl);
        this.requestTimeout = requestTimeout;
        this.objectMapper = new ObjectMapper();
        initializeObjectMapperConfiguration();
    }

    /**
     * Initialize a JsonRpcClient using a custom CloseableHttpClient instance.
     *
     * @param client            the custom HttpClient to use for all requests
     * @param jsonRpcBackendUrl the URL of the JSON-RPC backend. Usually of the form https://&lt;address&gt;.
     * @param requestTimeout    the timeout value for requests.
     * @see CloseableHttpClient CloseableHttpClient the client that will be used for dispatching requests
     * @see JsonRpcClient JsonRpcClient for more info on using this class
     */
    public JsonRpcClient(CloseableHttpClient client, URL jsonRpcBackendUrl, int requestTimeout) {
        httpClient = client;

        this.requestTimeout = requestTimeout;
        this.jsonRpcBackendUrl = formatBackendUrl(jsonRpcBackendUrl);

        this.objectMapper = new ObjectMapper();
        initializeObjectMapperConfiguration();
    }

    /**
     * Send a method call to xapi's backend. You need to provide the type of the data returned by a successful response.
     *
     * @param methodCall            the JSON-RPC xapi method call. e.g.: session.login_with_password
     * @param methodParameters      the parameters of the method call
     * @param responseTypeReference the type of the response, wrapped with a TypeReference
     * @param <T>                   The type of the response's payload. For instance, a map of opaque references to VM objects is expected when calling VM.get_all_records
     * @return a JsonRpcResponse object. If its error field is empty, the response was successful.
     * @throws JsonProcessingException if the request's payload or the response's payload cannot be written or read as valid JSON
     * @throws IOException             if an I/O error occurs when sending or receiving
     */
    public <T> JsonRpcResponse<T> sendRequest(String methodCall, Object[] methodParameters, TypeReference<T> responseTypeReference) throws IOException {
        var requestBody = objectMapper
                .writeValueAsString(new JsonRpcRequest(methodCall, methodParameters));

        var requestEntity = new StringEntity(requestBody, ContentType.APPLICATION_JSON);

        var requestConfig = RequestConfig.copy(defaultRequestConfig)
                .setConnectionRequestTimeout(this.requestTimeout, TimeUnit.SECONDS)
                .build();

        var request = new HttpPost(this.jsonRpcBackendUrl);
        request.setConfig(requestConfig);
        request.setEntity(requestEntity);

        return httpClient.execute(request, response -> {
            try (response) {
                var typeFactory = objectMapper.getTypeFactory();
                var responseObjectType = typeFactory.constructType(responseTypeReference.getType());
                var type = typeFactory.constructParametricType(JsonRpcResponse.class, responseObjectType);

                var responseContent = response.getEntity().getContent();
                return objectMapper.readValue(responseContent, type);
            }
        });
    }

    /**
     * Helper method to initialize jackson's ObjectMapper.
     */
    private void initializeObjectMapperConfiguration() {
        this.objectMapper.setDateFormat(new SimpleDateFormat("yyyyMMdd'T'HH:mm:ss'Z'"));
    }

    /**
     * Format input URL to the form protocol://host/jsonrpc
     *
     * @param url the input URL to format
     * @return a string version of a valid xen-api backend URL
     */
    private String formatBackendUrl(URL url) {
        // We only replace it when it's empty.
        // If the user purposely set the path
        // we use the given value even if incorrect
        if (!url.getPath().isEmpty()) {
            return url.getProtocol() + "://" + url.getHost() + JSON_BACKEND_PATH;
        }
        return url.toString();
    }
}
