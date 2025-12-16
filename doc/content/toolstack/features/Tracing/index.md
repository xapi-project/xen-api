+++
title = "Tracing"
+++

Tracing is a powerful tool for observing system behavior across multiple components, making it especially
useful for debugging and performance analysis in complex environments.

By integrating OpenTelemetry (a standard that unifies OpenTracing and OpenCensus) and the Zipkin v2 protocol,
XAPI enables efficient tracking and visualization of operations across internal and external systems.
This facilitates detailed analysis and improves collaboration between teams.

Tracing is commonly used in high-level applications such as web services. As a result, less widely-used or
non-web-oriented languages may lack dedicated libraries for distributed tracing (An OCaml implementation
has been developed specifically for XenAPI).

# How tracing works in XAPI

## Spans and Trace Context

- A *span* is the core unit of a trace, representing a single operation with a defined start and end time.
  Spans can contain sub-spans that represent child tasks. This helps identify bottlenecks or areas that
  can be parallelized.
  - A span can contain several contextual elements such as *tags* (key-value pairs),
    *events* (time-based data), and *errors*.
- The *TraceContext* HTTP standard defines how trace IDs and span contexts are propagated across systems,
  enabling full traceability of operations.

This data enables the creation of relationships between tasks and supports visualizations such as
architecture diagrams or execution flows. These help in identifying root causes of issues and bottlenecks,
and also assist newcomers in onboarding to the project.

## Configuration

- To enable tracing, you need to create an *Observer* object in XAPI. This can be done using the *xe* CLI:
  ```sh
  xe observer-create \
    name-label=<name> \
    enabled=true \
    components=xapi,xenopsd \
  ```
- By default, if you don't specify `enabled=true`, the observer will be disabled.
- To add an HTTP endpoint, make sure the server is up and running, then run:
  ```sh
  xe observer-param-set uuid=<OBSERVER_UUID> endpoints=bugtool,http://<jaeger-ip>:9411/api/v2/spans
  ```
  If you specify an invalid or unreachable HTTP endpoint, the configuration will fail.
- **components**: Specify which internal components (e.g., *xapi*, *xenopsd*) should be traced.
  Additional components are expected to be supported in future releases. An experimental *smapi* component
  is also available and requires additional configuration (explained below).

- **endpoints**: The observer can collect traces locally in */var/log/dt* or forward them to external
  visualization tools such as [Jaeger](https://www.jaegertracing.io/). Currently, only HTTP/S endpoints
  are supported, and they require additional configuration steps (see next section).

- To disable tracing you just need to set *enabled* to false:
  ```sh
  xe observer-param-set uuid=<OBSERVER_UUID> enabled=false
  ```

### Enabling smapi component

- *smapi* component is currently considered experimental and is filtered by default. To enable it, you must
  explicitly configure the following in **xapi.conf**:
  ```ini
  observer-experimental-components=""
  ```
  This tells XAPI that no components are considered experimental, thereby allowing *smapi* to be traced.
  A modification to **xapi.conf** requires a restart of the XAPI toolstack.

### Enabling HTTP/S endpoints

- By default HTTP and HTTPS endpoints are disabled. To enable them, add the following lines to **xapi.conf**:
  ```ini
  observer-endpoint-http-enabled=true
  observer-endpoint-https-enabled=true
  ```
  As with enabling *smapi* component, modifying **xapi.conf** requires a restart of the XAPI toolstack.
  *Note*: HTTPS endpoint support is available but not tested and may not work.

### Sending local trace to endpoint

By default, traces are generated locally in the `/var/log/dt` directory. You can copy or forward
these traces to another location or endpoint using the `xs-trace` tool. For example, if you have
a *Jaeger* server running locally, you can copy a trace to an endpoint by running:

```sh
xs-trace cp /var/log/dt/ http://127.0.0.1:9411/api/v2/spans
```

You will then be able to visualize the traces in Jaeger.

The `xs-trace` tool also supports trace files in `.ndjson` and compressed `.zst` formats, so
you can copy or forward these files directly as well.

### Tagging Trace Sessions for Easier Search

#### Specific attributes
To make trace logs easier to locate and analyze, it can be helpful to add custom attributes around the
execution of specific commands. For example:

```sh
# xe observer-param-set uuid=<OBSERVER_UUID> attributes:custom.random=1234
# xe vm-start ...
# xe observer-param-clear uuid=<OBSERVER_UUID> param-name=attributes param-key=custom.random
```

This technique adds a temporary attribute, *custom.random=1234*, which will appear in the generated trace
spans, making it easier to search for specific activity in trace visualisation tools. It may also be possible
to achieve similar tagging using baggage parameters directly in individual *xe* commands, but this approach
is currently undocumented.

#### Baggage

*Baggage*, contextual information that resides alongside the context, is supported. This means you can run
the following command:

```sh
BAGGAGE="mybaggage=apples" xe vm-list
```

You will be able to search for tags `mybaggage=apples`.

#### Traceparent

Another way to assist in trace searching is to use the `TRACEPARENT` HTTP header. It is an HTTP header field that
identifies the incoming request. It has a [specific format](https://www.w3.org/TR/trace-context/#traceparent-header)
and it is supported by **XAPI**. Once generated you can run command as:

```sh
TRACEPARENT="00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01" xe vm-list
```

And you will be able to look for trace *4bf92f3577b34da6a3ce929d0e0e4736*.

### Links

- [Opentelemetry](https://opentelemetry.io/)
- [Trace Context](https://www.w3.org/TR/trace-context/)
- [Baggage](https://opentelemetry.io/docs/concepts/signals/baggage/)
- [Ocaml opentelemetry module](https://ocaml.org/p/opentelemetry/latest)
