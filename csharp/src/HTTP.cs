/*
 * Copyright (c) Citrix Systems, Inc.
 * All rights reserved.
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
using System.IO;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Net.Security;
using System.Security.Cryptography.X509Certificates;

namespace XenAPI
{
    public partial class HTTP
    {
        public class TooManyRedirectsException : Exception
        {
            private readonly int redirect;
            private readonly Uri uri;

            public TooManyRedirectsException(int redirect, Uri uri)
            {
                this.redirect = redirect;
                this.uri = uri;
            }
        }

        public class BadServerResponseException : Exception
        {
            public BadServerResponseException(string msg)
                : base(msg)
            { }
        }

        public class CancelledException : Exception
        {
        }

        public delegate bool FuncBool();
        public delegate void UpdateProgressDelegate(int percent);
        public delegate void DataCopiedDelegate(long bytes);

        // Size of byte buffer used for GETs and PUTs
        // (not the socket rx buffer)
        public const int BUFFER_SIZE = 32 * 1024;
        public const int MAX_REDIRECTS = 10;

        public const int DEFAULT_HTTPS_PORT = 443;

        #region Helper functions

        private static void WriteLine(String txt, Stream stream)
        {
            byte[] bytes = System.Text.Encoding.ASCII.GetBytes(String.Format("{0}\r\n", txt));
            stream.Write(bytes, 0, bytes.Length);
        }

        private static void WriteLine(Stream stream)
        {
            WriteLine("", stream);
        }

        private static string ReadLine(Stream stream)
        {
            System.Text.StringBuilder result = new StringBuilder();
            while (true)
            {
                int b = stream.ReadByte();
                if (b == -1)
                    throw new EndOfStreamException();
                char c = Convert.ToChar(b);
                result.Append(c);
                if (c == '\n')
                    return result.ToString();
            }
        }

        /// <summary>
        /// Read HTTP headers, doing any redirects as necessary
        /// </summary>
        /// <param name="stream"></param>
        /// <returns>True if a redirect has occurred - headers will need to be resent.</returns>
        private static bool ReadHttpHeaders(ref Stream stream, IWebProxy proxy, bool nodelay, int timeout_ms)
        {
            string response = ReadLine(stream);
            int code = getResultCode(response);

            switch (code)
            {
                case 200:
                    break;

                case 302:
                    string url = "";
                    while (true)
                    {
                        response = ReadLine(stream);
                        if (response.StartsWith("Location: "))
                            url = response.Substring(10);
                        if (response.Equals("\r\n") || response.Equals("\n") || response.Equals(""))
                            break;
                    }
                    Uri redirect = new Uri(url.Trim());
                    stream.Close();
                    stream = ConnectStream(redirect, proxy, nodelay, timeout_ms);
                    return true; // headers need to be sent again

                default:
                    if (response.EndsWith("\r\n"))
                        response = response.Substring(0, response.Length - 2);
                    else if (response.EndsWith("\n"))
                        response = response.Substring(0, response.Length - 1);
                    stream.Close();
                    throw new BadServerResponseException(string.Format("Received error code {0} from the server", response));
            }

            while (true)
            {
                string line = ReadLine(stream);
                if (System.Text.RegularExpressions.Regex.Match(line, "^\\s*$").Success)
                    break;
            }

            return false;
        }

        public static int getResultCode(string line)
        {
            string[] bits = line.Split(new char[] { ' ' });
            return (bits.Length < 2 ? 0 : Int32.Parse(bits[1]));
        }

        public static bool UseSSL(Uri uri)
        {
            return uri.Scheme == "https" || uri.Port == DEFAULT_HTTPS_PORT;
        }

        private static bool ValidateServerCertificate(
              object sender,
              X509Certificate certificate,
              X509Chain chain,
              SslPolicyErrors sslPolicyErrors)
        {
            return true;
        }

        public static long CopyStream(Stream inStream, Stream outStream,
            DataCopiedDelegate progressDelegate, FuncBool cancellingDelegate)
        {
            long bytesWritten = 0;
            byte[] buffer = new byte[BUFFER_SIZE];
            DateTime lastUpdate = DateTime.Now;

            while (cancellingDelegate == null || !cancellingDelegate())
            {
                int bytesRead = inStream.Read(buffer, 0, buffer.Length);
                if (bytesRead == 0)
                    break;
                outStream.Write(buffer, 0, bytesRead);
                bytesWritten += bytesRead;

                if (progressDelegate != null &&
                    DateTime.Now - lastUpdate > TimeSpan.FromMilliseconds(500))
                {
                    progressDelegate(bytesWritten);
                    lastUpdate = DateTime.Now;
                }
            }

            if (cancellingDelegate != null && cancellingDelegate())
                throw new CancelledException();

            if (progressDelegate != null)
                progressDelegate(bytesWritten);

            return bytesWritten;
        }

        /// <summary>
        /// Build a URI from a hostname, a path, and some query arguments
        /// </summary>
        /// <param name="args">An even-length array, alternating argument names and values</param>
        /// <returns></returns>
        public static Uri BuildUri(string hostname, string path, params object[] args)
        {
            // The last argument may be an object[] in its own right, in which case we need
            // to flatten the array.
            List<object> flatargs = new List<object>();
            foreach (object arg in args)
            {
                if (arg is IEnumerable<object>)
                    flatargs.AddRange((IEnumerable<object>)arg);
                else
                    flatargs.Add(arg);
            }

            UriBuilder uri = new UriBuilder();
            uri.Scheme = "https";
            uri.Port = DEFAULT_HTTPS_PORT;
            uri.Host = hostname;
            uri.Path = path;

            StringBuilder query = new StringBuilder();
            for (int i = 0; i < flatargs.Count - 1; i += 2)
            {
                string kv;

                // If the argument is null, don't include it in the URL
                if (flatargs[i + 1] == null)
                    continue;

                // bools are special because some xapi calls use presence/absence and some
                // use "b=true" (not "True") and "b=false". But all accept "b=true" or absent.
                if (flatargs[i + 1] is bool)
                {
                    if (!((bool)flatargs[i + 1]))
                        continue;
                    kv = flatargs[i] + "=true";
                }
                else
                    kv = flatargs[i] + "=" + Uri.EscapeDataString(flatargs[i + 1].ToString());

                if (query.Length != 0)
                    query.Append('&');
                query.Append(kv);
            }
            uri.Query = query.ToString();

            return uri.Uri;
        }

        #endregion

        private static NetworkStream ConnectSocket(Uri uri, bool nodelay, int timeout_ms)
        {
            AddressFamily addressFamily = uri.HostNameType == UriHostNameType.IPv6
                                              ? AddressFamily.InterNetworkV6
                                              : AddressFamily.InterNetwork;
            Socket socket =
                new Socket(addressFamily, SocketType.Stream, ProtocolType.Tcp);
            socket.NoDelay = nodelay;
            //socket.ReceiveBufferSize = 64 * 1024;
            socket.ReceiveTimeout = timeout_ms;
            socket.SendTimeout = timeout_ms;
            socket.Connect(uri.Host, uri.Port);

            return new NetworkStream(socket, true);
        }

        /// <summary>
        /// This function will connect a stream to a uri (host and port), 
        /// negotiating proxies and SSL
        /// </summary>
        /// <param name="uri"></param>
        /// <param name="timeout_ms">Timeout, in ms. 0 for no timeout.</param>
        /// <returns></returns>
        public static Stream ConnectStream(Uri uri, IWebProxy proxy, bool nodelay, int timeout_ms)
        {
            IMockWebProxy mockProxy = proxy != null ? proxy as IMockWebProxy : null;
            if (mockProxy != null)
                return mockProxy.GetStream(uri);

            Stream stream;
            bool useProxy = proxy != null && !proxy.IsBypassed(uri);

            if (useProxy)
            {
                Uri proxyURI = proxy.GetProxy(uri);
                stream = ConnectSocket(proxyURI, nodelay, timeout_ms);
            }
            else
            {
                stream = ConnectSocket(uri, nodelay, timeout_ms);
            }

            try
            {
                if (useProxy)
                {
                    string line = String.Format("CONNECT {0}:{1} HTTP/1.0", uri.Host, uri.Port);

                    WriteLine(line, stream);
                    WriteLine(stream);

                    ReadHttpHeaders(ref stream, proxy, nodelay, timeout_ms);
                }

                if (UseSSL(uri))
                {
                    SslStream sslStream = new SslStream(stream, false,
                        new RemoteCertificateValidationCallback(ValidateServerCertificate), null);
                    sslStream.AuthenticateAsClient("");

                    stream = sslStream;
                }

                return stream;
            }
            catch
            {
                stream.Close();
                throw;
            }
        }

        private static Stream DO_HTTP(Uri uri, IWebProxy proxy, bool nodelay, int timeout_ms, params string[] headers)
        {
            Stream stream = ConnectStream(uri, proxy, nodelay, timeout_ms);

            int redirects = 0;

            do
            {
                if (redirects > MAX_REDIRECTS)
                    throw new TooManyRedirectsException(redirects, uri);

                redirects++;

                foreach (string header in headers)
                    WriteLine(header, stream);
                WriteLine(stream);

                stream.Flush();
            }
            while (ReadHttpHeaders(ref stream, proxy, nodelay, timeout_ms));

            return stream;
        }

        //
        // The following functions do all the HTTP headers related stuff
        // returning the stream ready for use
        //

        public static Stream CONNECT(Uri uri, IWebProxy proxy, String session, int timeout_ms)
        {
            return DO_HTTP(uri, proxy, true, timeout_ms,
                string.Format("CONNECT {0} HTTP/1.0", uri.PathAndQuery),
                string.Format("Host: {0}", uri.Host),
                string.Format("Cookie: session_id={0}", session));
        }

        public static Stream PUT(Uri uri, IWebProxy proxy, long ContentLength, int timeout_ms)
        {
            return DO_HTTP(uri, proxy, false, timeout_ms,
                string.Format("PUT {0} HTTP/1.0", uri.PathAndQuery),
                string.Format("Content-Length: {0}", ContentLength));
        }

        public static Stream GET(Uri uri, IWebProxy proxy, int timeout_ms)
        {
            return DO_HTTP(uri, proxy, false, timeout_ms,
                string.Format("GET {0} HTTP/1.0", uri.PathAndQuery));
        }

        /// <summary>
        /// A general HTTP PUT method, with delegates for progress and cancelling. May throw various exceptions.
        /// </summary>
        /// <param name="progressDelegate">Delegate called periodically (500ms) with percent complete</param>
        /// <param name="cancellingDelegate">Delegate called periodically to see if need to cancel</param>
        /// <param name="uri">URI to PUT to</param>
        /// <param name="proxy">A proxy to handle the HTTP connection</param>
        /// <param name="path">Path to file to put</param>
        /// <param name="timeout_ms">Timeout for the connection in ms. 0 for no timeout.</param>
        public static void Put(UpdateProgressDelegate progressDelegate, FuncBool cancellingDelegate,
            Uri uri, IWebProxy proxy, string path, int timeout_ms)
        {
            using (Stream fileStream = new FileStream(path, FileMode.Open, FileAccess.Read),
                requestStream = PUT(uri, proxy, fileStream.Length, timeout_ms))
            {
                long len = fileStream.Length;
                DataCopiedDelegate dataCopiedDelegate = delegate(long bytes)
                    {
                        if (progressDelegate != null && len > 0)
                            progressDelegate((int)((bytes * 100) / len));
                    };

                CopyStream(fileStream, requestStream, dataCopiedDelegate, cancellingDelegate);
            }
        }

        /// <summary>
        /// A general HTTP GET method, with delegates for progress and cancelling. May throw various exceptions.
        /// </summary>
        /// <param name="dataRxDelegate">Delegate called periodically (500 ms) with the number of bytes transferred</param>
        /// <param name="cancellingDelegate">Delegate called periodically to see if need to cancel</param>
        /// <param name="uri">URI to GET from</param>
        /// <param name="proxy">A proxy to handle the HTTP connection</param>
        /// <param name="path">Path to file to receive the data</param>
        /// <param name="timeout_ms">Timeout for the connection in ms. 0 for no timeout.</param>
        public static void Get(DataCopiedDelegate dataCopiedDelegate, FuncBool cancellingDelegate,
            Uri uri, IWebProxy proxy, string path, int timeout_ms)
        {
            string tmpFile = Path.GetTempFileName();
            try
            {
                using (Stream fileStream = new FileStream(tmpFile, FileMode.Create, FileAccess.Write, FileShare.None),
                    downloadStream = GET(uri, proxy, timeout_ms))
                {
                    CopyStream(downloadStream, fileStream, dataCopiedDelegate, cancellingDelegate);
                    fileStream.Flush();
                }

                File.Delete(path);
                File.Move(tmpFile, path);
            }
            finally
            {
                File.Delete(tmpFile);
            }
        }
    }
}
