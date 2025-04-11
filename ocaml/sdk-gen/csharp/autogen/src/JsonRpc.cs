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

using System;
using System.Collections.Generic;
#if (NET462_OR_GREATER || NETSTANDARD2_0_OR_GREATER)
using System.Diagnostics;
#endif
using System.IO;
using System.Net;
using System.Net.Security;
using System.Threading;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;


namespace XenAPI
{
    public enum JsonRpcVersion
    {
        v1,
        v2
    }

    internal abstract class JsonRequest
    {
        protected JsonRequest(int id, string method, JToken parameters)
        {
            this.Id = id;
            this.Method = method;
            this.Parameters = parameters;
        }

        public static JsonRequest Create(JsonRpcVersion jsonRpcVersion, int id, string method, JToken parameters)
        {
            switch (jsonRpcVersion)
            {
                case JsonRpcVersion.v2:
                    return new JsonRequestV2(id, method, parameters);
                default:
                    return new JsonRequestV1(id, method, parameters);
            }
        }

        /// <summary>
        /// Unique call id. Can be null in JSON_RPC v2.0, but xapi disallows it.
        /// </summary>
        [JsonProperty("id", Required = Required.Always)]
        public int Id { get; private set; }

        /// <summary>
        /// The API function to call.
        /// </summary>
        [JsonProperty("method", Required = Required.Always)]
        public string Method { get; private set; }

        /// <summary>
        /// Any parameters, optional in JSON-RPC v2.0.
        /// </summary>
        [JsonProperty("params", Required = Required.Always)]
        public JToken Parameters { get; private set; }

        public override string ToString()
        {
            return JsonConvert.SerializeObject(this, Formatting.Indented);
        }
    }

    internal class JsonRequestV1 : JsonRequest
    {
        public JsonRequestV1(int id, string method, JToken parameters)
            : base(id, method, parameters)
        {
        }
    }

    internal class JsonRequestV2 : JsonRequest
    {
        public JsonRequestV2(int id, string method, JToken parameters)
            : base(id, method, parameters)
        {
        }

        [JsonProperty("jsonrpc", Required = Required.Always)]
        public string JsonRPC
        {
            get { return "2.0"; }
        }
    }


    internal abstract class JsonResponse<T>
    {
        [JsonProperty("id", Required = Required.AllowNull)] public int Id = 0;

        [JsonProperty("result", Required = Required.Default)] public T Result = default(T);

        public override string ToString()
        {
            return JsonConvert.SerializeObject(this, Formatting.Indented);
        }
    }

    internal class JsonResponseV1<T> : JsonResponse<T>
    {
        [JsonProperty("error", Required = Required.AllowNull)] public JToken Error = null;
    }

    internal class JsonResponseV2<T> : JsonResponse<T>
    {
        [JsonProperty("error", Required = Required.DisallowNull)] public JsonResponseV2Error Error = null;

        [JsonProperty("jsonrpc", Required = Required.Always)] public string JsonRpc = null;
    }

    internal class JsonResponseV2Error
    {
        [JsonProperty("code", Required = Required.Always)] public int Code = 0;

        [JsonProperty("message", Required = Required.Always)] public string Message = null;

        [JsonProperty("data", Required = Required.Default)] public JToken Data = null;

        public override string ToString()
        {
            return JsonConvert.SerializeObject(this, Formatting.Indented);
        }
    }


    public partial class JsonRpcClient
    {
        private int _globalId;

#if (NET462_OR_GREATER || NETSTANDARD2_0_OR_GREATER)
        private static readonly Type ClassType = typeof(JsonRpcClient);
        private static readonly System.Reflection.AssemblyName ClassAssemblyName= ClassType?.Assembly?.GetName();
        private static readonly ActivitySource source = new ActivitySource(ClassAssemblyName.Name + "." + ClassType?.FullName, ClassAssemblyName.Version?.ToString());

        // Follow naming conventions from OpenTelemetry.SemanticConventions
        // Not yet on NuGet though:
        // dotnet add package OpenTelemetry.SemanticConventions
        private static class RpcAttributes {
            public const string AttributeRpcMethod = "rpc.method";
            public const string AttributeRpcSystem = "rpc.system";
            public const string AttributeRpcService = "rpc.service";
            public const string AttributeRpcJsonrpcErrorCode = "rpc.jsonrpc.error_code";
            public const string AttributeRpcJsonrpcErrorMessage = "rpc.jsonrpc.error_message";
            public const string AttributeRpcJsonrpcRequestId = "rpc.jsonrpc.request_id";
            public const string AttributeRpcJsonrpcVersion = "rpc.jsonrpc.version";

            public const string AttributeRpcMessageType = "rpc.message.type";
            public static class RpcMessageTypeValues
            {
                public const string Sent = "SENT";

                public const string Received = "RECEIVED";
            }
        }

        private static class ServerAttributes {
            public const string AttributeServerAddress = "server.address";
        }

        // not part of the SemanticConventions package
        private const string ValueJsonRpc = "jsonrpc";
        private const string EventRpcMessage = "rpc.message";
#endif

        public JsonRpcClient(string baseUrl)
        {
            Url = baseUrl;
            JsonRpcUrl = new Uri(new Uri(baseUrl), "/jsonrpc").ToString();
            JsonRpcVersion = JsonRpcVersion.v1;
        }

        /// <summary>
        /// Fired when a request has been serialized taking as a parameter the call name in RELEASE and the
        /// Json string in DEBUG mode.
        /// IMPORTANT: the latter may contain sensitive data, so handle it carefully.
        /// </summary>
        public event Action<string> RequestEvent;

        public JsonRpcVersion JsonRpcVersion { get; set; }
        public string UserAgent { get; set; }
        public bool KeepAlive { get; set; }
        public IWebProxy WebProxy { get; set; }
        public int Timeout { get; set; }
        public string ConnectionGroupName { get; set; }
        public Version ProtocolVersion { get; set; }
        public bool Expect100Continue { get; set; }
        public bool AllowAutoRedirect { get; set; }
        public bool PreAuthenticate { get; set; }
        public CookieContainer Cookies { get; set; }
        public RemoteCertificateValidationCallback ServerCertificateValidationCallback { get; set; }
        public Dictionary<string, string> RequestHeaders { get; set; }
        public Dictionary<string, string> ResponseHeaders { get; set; }

        public string Url { get; private set; }

        public string JsonRpcUrl { get; private set; }

        private void Rpc(string callName, JToken parameters, JsonSerializer serializer)
        {
            Rpc<object>(callName, parameters, serializer);
        }

        protected virtual T Rpc<T>(string callName, JToken parameters, JsonSerializer serializer)
        {
            // Note that the following method handles an overflow condition by wrapping.
            // If the _globalId reaches Int32.MaxValue, _globalId + 1 starts over from
            // Int32.MinValue and no exception is thrown.
            var id = Interlocked.Increment(ref _globalId);

            JsonRequest request = JsonRequest.Create(JsonRpcVersion, id, callName, parameters);

            // for performance reasons it's preferable to deserialize directly
            // from the Stream rather than allocating strings inbetween
            // therefore the latter will be done only in DEBUG mode
            using (var postStream = new MemoryStream())
            {
#if (NET462_OR_GREATER || NETSTANDARD2_0_OR_GREATER)
              // the semantic convention is $package.$service/$method
              using (Activity activity = source.CreateActivity("XenAPI/" + callName, ActivityKind.Client))
              {
                 // .NET 5 would use W3C format for the header by default but we build for .Net 4.x still
                activity?.SetIdFormat(ActivityIdFormat.W3C);
                activity?.Start();
                // Set the fields described in the OpenTelemetry Semantic Conventions:
                // https://web.archive.org/web/20250119181511/https://opentelemetry.io/docs/specs/semconv/rpc/json-rpc/
                // https://web.archive.org/web/20241113162246/https://opentelemetry.io/docs/specs/semconv/rpc/rpc-spans/
                activity?.SetTag(RpcAttributes.AttributeRpcSystem, ValueJsonRpc);
                activity?.SetTag(ServerAttributes.AttributeServerAddress, new Uri(Url).Host);
                activity?.SetTag(RpcAttributes.AttributeRpcMethod, callName);
                activity?.SetTag(RpcAttributes.AttributeRpcJsonrpcRequestId, id.ToString());
#endif
                using (var sw = new StreamWriter(postStream))
                {
#if DEBUG
                    var settings = CreateSettings(serializer.Converters);
                    string jsonReq = JsonConvert.SerializeObject(request, settings);
                    if (RequestEvent != null)
                        RequestEvent(jsonReq);
                    sw.Write(jsonReq);
#else
                    if (RequestEvent != null)
                        RequestEvent(callName);
                    serializer.Serialize(sw, request);
#endif
                    sw.Flush();
                    postStream.Seek(0, SeekOrigin.Begin);

                    using (var responseStream = new MemoryStream())
                    {
                        PerformPostRequest(postStream, responseStream);
                        responseStream.Position = 0;

                        using (var responseReader = new StreamReader(responseStream))
                        {
                            switch (JsonRpcVersion)
                            {
                                case JsonRpcVersion.v2:
#if (NET462_OR_GREATER || NETSTANDARD2_0_OR_GREATER)
                                    activity?.SetTag(RpcAttributes.AttributeRpcJsonrpcVersion, "2.0");
#endif
#if DEBUG
                                    string json2 = responseReader.ReadToEnd();
                                    var res2 = JsonConvert.DeserializeObject<JsonResponseV2<T>>(json2, settings);
#else
                                    var res2 = (JsonResponseV2<T>)serializer.Deserialize(responseReader, typeof(JsonResponseV2<T>));
#endif

                                    if (res2.Error != null)
                                    {
                                        var descr = new List<string> { res2.Error.Message };
                                        descr.AddRange(res2.Error.Data.ToObject<string[]>());
#if (NET462_OR_GREATER || NETSTANDARD2_0_OR_GREATER)
                                        activity?.SetTag(RpcAttributes.AttributeRpcJsonrpcErrorCode, res2.Error.Code);
                                        activity?.SetTag(RpcAttributes.AttributeRpcJsonrpcErrorMessage, descr);
                                        activity?.SetStatus(ActivityStatusCode.Error);
#endif
                                        throw new Failure(descr);
                                    }

#if (NET462_OR_GREATER || NETSTANDARD2_0_OR_GREATER)
                                    activity?.SetStatus(ActivityStatusCode.Ok);
#endif
                                    return res2.Result;
                                default:
#if (NET462_OR_GREATER || NETSTANDARD2_0_OR_GREATER)
                                    activity?.SetTag(RpcAttributes.AttributeRpcJsonrpcVersion, "1.0");
#endif
#if DEBUG
                                    string json1 = responseReader.ReadToEnd();
                                    var res1 = JsonConvert.DeserializeObject<JsonResponseV1<T>>(json1, settings);
#else
                                    var res1 = (JsonResponseV1<T>)serializer.Deserialize(responseReader, typeof(JsonResponseV1<T>));
#endif

                                    if (res1.Error != null)
                                    {
                                        var errorArray = res1.Error.ToObject<string[]>();
                                        if (errorArray != null) {
#if (NET462_OR_GREATER || NETSTANDARD2_0_OR_GREATER)
                                            activity?.SetStatus(ActivityStatusCode.Error);
                                            // we can't be sure whether we'll have a Code here
                                            // the exact format of an error object is not specified in JSONRPC v1
                                            activity?.SetTag(RpcAttributes.AttributeRpcJsonrpcErrorMessage, errorArray.ToString());
#endif
                                            throw new Failure(errorArray);
                                        }
                                    }
#if (NET462_OR_GREATER || NETSTANDARD2_0_OR_GREATER)
                                    activity?.SetStatus(ActivityStatusCode.Ok);
#endif
                                    return res1.Result;
                            }
                        }
                    }
                }
#if (NET462_OR_GREATER || NETSTANDARD2_0_OR_GREATER)
              }
#endif
            }
        }


        protected virtual void PerformPostRequest(Stream postStream, Stream responseStream)
        {
            var webRequest = (HttpWebRequest)WebRequest.Create(JsonRpcUrl);
            webRequest.Method = "POST";
            webRequest.ContentType = "application/json";
            webRequest.Accept = "application/json";
            webRequest.Timeout = Timeout;
            webRequest.Proxy = WebProxy;
            webRequest.KeepAlive = KeepAlive;
            webRequest.UserAgent = UserAgent;
            webRequest.ConnectionGroupName = ConnectionGroupName;
            webRequest.ProtocolVersion = ProtocolVersion ?? webRequest.ProtocolVersion;
            webRequest.ServicePoint.Expect100Continue = Expect100Continue;
            webRequest.AllowAutoRedirect = AllowAutoRedirect;
            webRequest.PreAuthenticate = PreAuthenticate;
            webRequest.AllowWriteStreamBuffering = true;
            webRequest.CookieContainer = Cookies ?? webRequest.CookieContainer ?? new CookieContainer();
            webRequest.ServerCertificateValidationCallback = ServerCertificateValidationCallback ?? ServicePointManager.ServerCertificateValidationCallback;

            if (RequestHeaders != null)
            {
                foreach (var header in RequestHeaders)
                    webRequest.Headers.Add(header.Key, header.Value);
            }

#if (NET462_OR_GREATER || NETSTANDARD2_0_OR_GREATER)
            // propagate W3C traceparent and tracestate
            // HttpClient would do this automatically on .NET 5,
            // and .NET 6 would provide even more control over this: https://blog.ladeak.net/posts/opentelemetry-net6-httpclient
            // the caller must ensure that the activity is in W3C format (by inheritance or direct setting)
            var activity = Activity.Current;
            if (activity != null && activity.IdFormat == ActivityIdFormat.W3C)
            {
                webRequest.Headers.Add("traceparent", activity.Id);
                var state = activity.TraceStateString;
                if (state?.Length > 0)
                {
                    webRequest.Headers.Add("tracestate", state);
                }
            }
#endif

            using (var str = webRequest.GetRequestStream())
            {
                postStream.CopyTo(str);
                str.Flush();
            }

#if (NET462_OR_GREATER || NETSTANDARD2_0_OR_GREATER)
            if (activity != null) {
                var tags =  new ActivityTagsCollection{
                    { RpcAttributes.AttributeRpcMessageType, RpcAttributes.RpcMessageTypeValues.Sent }
                };
                activity.AddEvent(new ActivityEvent(EventRpcMessage, DateTimeOffset.Now, tags));
            }
#endif

            HttpWebResponse webResponse = null;
            try
            {
                webResponse = (HttpWebResponse)webRequest.GetResponse();

                var newResponseHeaders = new Dictionary<string, string>();

                if (webResponse.Headers != null)
                {
                    var keys = webResponse.Headers.AllKeys;
                    foreach (var key in keys)
                        newResponseHeaders.Add(key, string.Join(",", webResponse.Headers.Get(key)));
                }

                ResponseHeaders = newResponseHeaders;

                if (webResponse.StatusCode != HttpStatusCode.OK)
                    throw new WebException(webResponse.StatusCode.ToString());

                using (var str = webResponse.GetResponseStream())
                {
                    if (str == null)
                        throw new WebException();

                    str.CopyTo(responseStream);
                    responseStream.Flush();
                }
                
#if (NET462_OR_GREATER || NETSTANDARD2_0_OR_GREATER)
            if (activity != null) {
                var tags =  new ActivityTagsCollection{
                    { RpcAttributes.AttributeRpcMessageType, RpcAttributes.RpcMessageTypeValues.Received }
                };
                activity.AddEvent(new ActivityEvent(EventRpcMessage, DateTimeOffset.Now, tags));
            }
#endif

            }
            finally
            {
                RequestHeaders = null;
                webResponse?.Dispose();
            }
        }

        private JsonSerializerSettings CreateSettings(IList<JsonConverter> converters)
        {
            return new JsonSerializerSettings
            {
#if DEBUG
                Formatting = Formatting.Indented,
#endif
                Converters = converters,
                DateParseHandling = DateParseHandling.None,
                NullValueHandling = NullValueHandling.Ignore
            };
        }

        private JsonSerializer CreateSerializer(IList<JsonConverter> converters)
        {
            var settings = CreateSettings(converters);
            return JsonSerializer.Create(settings);
        }
    }
}
