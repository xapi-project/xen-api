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
import org.apache.hc.core5.util.Timeout;

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
 * <br />
 * By default, the timeout for requests is set to {@value #DEFAULT_REQUEST_TIMEOUT}. The default timeout for connecting to the
 * JSON-RPC backend is set to {@value #DEFAULT_CONNECTION_TIMEOUT} seconds. The maximum number of concurrent connections handled
 * by the underlying {@link PoolingHttpClientConnectionManager} is {@value #MAX_CONCURRENT_CONNECTIONS}.
 *
 * @see CloseableHttpClient CloseableHttpClient is used to make requests and connect to the backend
 * @see ObjectMapper ObjectMapper is used to marshall requests and responses
 */
public class JsonRpcClient {
    private static final int DEFAULT_REQUEST_TIMEOUT = 600;
    private static final int DEFAULT_CONNECTION_TIMEOUT = 5;

    private static final int MAX_CONCURRENT_CONNECTIONS = 10;

    private static final String JSON_BACKEND_PATH = "/jsonrpc";

    private final CloseableHttpClient httpClient;
    private final String jsonRpcBackendUrl;
    private final ObjectMapper objectMapper;
    private final RequestConfig defaultRequestConfig = RequestConfig.custom()
            .setCookieSpec(StandardCookieSpec.IGNORE)
            .build();
    private int requestTimeout;
    private PoolingHttpClientConnectionManager connectionManager;

    //region Constructors

    /**
     * Create a JsonRpcClient with default settings.
     *
     * @param jsonRpcBackendUrl the URL of the JSON-RPC backend. Usually of the form https://&lt;address&gt;.
     * @see JsonRpcClient JsonRpcClient for more info on using this class
     */
    public JsonRpcClient(URL jsonRpcBackendUrl) {
        var connectionConfig = ConnectionConfig
                .custom()
                .setConnectTimeout(DEFAULT_CONNECTION_TIMEOUT, TimeUnit.SECONDS)
                .build();

        this.connectionManager = new PoolingHttpClientConnectionManager();
        connectionManager.setDefaultConnectionConfig(connectionConfig);
        connectionManager.setMaxTotal(MAX_CONCURRENT_CONNECTIONS);

        this.httpClient = HttpClients
                .custom()
                .setConnectionManager(connectionManager)
                .build();
        this.jsonRpcBackendUrl = formatBackendUrl(jsonRpcBackendUrl);
        this.requestTimeout = DEFAULT_REQUEST_TIMEOUT;
        this.objectMapper = new ObjectMapper();
        initializeObjectMapperConfiguration();
    }

    /**
     * Initialize a JsonRpcClient using a custom CloseableHttpClient instance.
     *
     * @param client            the custom HttpClient to use for all requests
     * @param jsonRpcBackendUrl the URL of the JSON-RPC backend. Usually of the form https://&lt;address&gt;.
     * @see CloseableHttpClient CloseableHttpClient the client that will be used for dispatching requests
     * @see JsonRpcClient JsonRpcClient for more info on using this class
     */
    public JsonRpcClient(CloseableHttpClient client, URL jsonRpcBackendUrl) {
        httpClient = client;
        this.jsonRpcBackendUrl = formatBackendUrl(jsonRpcBackendUrl);
        this.objectMapper = new ObjectMapper();
        initializeObjectMapperConfiguration();
    }

    //endregion

    //region Public Setters

    /**
     * Set the timeout in seconds for every request made by this client.
     * If not set the value defaults to {@value #DEFAULT_REQUEST_TIMEOUT}.
     *
     * @param requestTimeout the timeout value in seconds
     * @see org.apache.hc.client5.http.config.RequestConfig.Builder#setConnectionRequestTimeout(long, TimeUnit)
     */
    public void setRequestTimeout(int requestTimeout) {
        this.requestTimeout = requestTimeout;
    }

    /**
     * Set the connection timeout in seconds for this client's {@link org.apache.hc.client5.http.impl.io.PoolingHttpClientConnectionManager}.
     * If not set the value defaults to {@value #DEFAULT_CONNECTION_TIMEOUT}.
     *
     * @param connectionTimeout the client's connection timeout in seconds.
     * @see org.apache.hc.client5.http.config.ConnectionConfig.Builder#setConnectTimeout(Timeout)
     */
    public void setConnectionTimeout(int connectionTimeout) {
        connectionManager.setDefaultConnectionConfig(ConnectionConfig
                .custom()
                .setConnectTimeout(connectionTimeout, TimeUnit.SECONDS)
                .build()
        );
    }

    /**
     * Set the maximum number of connections that this client's {@link org.apache.hc.client5.http.impl.io.PoolingHttpClientConnectionManager} will keep open.
     * If not set the value defaults to {@value #MAX_CONCURRENT_CONNECTIONS}.
     *
     * @param maxConcurrentConnections the maximum number of connections managed by the connection manager
     * @see org.apache.hc.core5.pool.ConnPoolControl#setMaxTotal(int)
     */
    public void setMaxConcurrentConnections(int maxConcurrentConnections) {
        connectionManager.setMaxTotal(maxConcurrentConnections);
    }
    //endregion

    /**
     * Send a method call to xapi's backend. You need to provide the type of the data returned by a successful response.
     *
     * @param methodCall            the JSON-RPC xapi method call. e.g.: session.login_with_password
     * @param methodParameters      the parameters of the method call
     * @param responseTypeReference the type of the response, wrapped with a TypeReference
     * @param <T>                   The type of the response's payload. For instance, a map of opaque references to VM objects is expected when calling VM.get_all_records
     * @return a {@link JsonRpcResponse} object. If its error field is empty, the response was successful.
     * @throws JsonProcessingException if the request's payload or the response's payload cannot be written or read as valid JSON
     * @throws IOException             if an I/O error occurs when sending or receiving
     */
    protected <T> JsonRpcResponse<T> sendRequest(String methodCall, Object[] methodParameters, TypeReference<T> responseTypeReference) throws IOException {
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
