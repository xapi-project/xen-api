# Prometheus stats

This library supports exposing disk statistics in "prometheus" format.

In a client application, stats can be exposed by instantiating the
[Prometheus_app](https://github.com/mirage/prometheus/blob/master/app/prometheus_app.mli)
functor, see
[this example in the mirage/prometheus repo[(https://github.com/mirage/prometheus/blob/master/examples/example.ml)
or
[this example in the moby/hyperkit repo](https://github.com/moby/hyperkit/blob/70205a6d5143340299a679af259f70dfcd7cf8a4/src/lib/mirage_block_ocaml.ml#L188).

Once exposed, stats can be gathered by an instance of [prometheus](https://prometheus.io) and
then rendered into dashboards by tools like [grafana](https://grafana.com).

## Example

Docker for Mac uses this qcow implementation and therefore has prometheus
support. First install the latest experimental version from the
[master branch](https://download-stage.docker.com/mac/master/Docker.dmg).

Start the application once, and then shut it down again -- this will create
the initial configuration.

Expose metrics on `0.0.0.0:9090` by:
```
cd ~/Library/Containers/com.docker.docker/Data/database/
git reset --hard
mkdir -p com.docker.driver.amd64-linux/disk
echo -n "tcp:9090" > com.docker.driver.amd64-linux/disk/stats
git add com.docker.driver.amd64-linux/disk/stats
git commit -s -m 'Expose stats on port 9090 on all interfaces'
```

Test the metrics are working by:
```
curl http://localhost:9090/metrics
```

Download [prometheus.yml](https://raw.githubusercontent.com/mirage/ocaml-qcow/master/doc/prometheus.yml)

Next run a prometheus server with:
```
docker run -d -p 9091:9090 -v $(pwd)/prometheus.yml:/etc/prometheus/prometheus.yml prom/prometheus
```
There should now be a prometheus server on port 9091. If you browse http://localhost:9091 and
select the "Status" menu and then "Targets" you should see the target marked as "UP".

Next run a grafana instance with:
```
docker run -d --name=grafana -p 3000:3000 grafana/grafana
```
Load http://localhost:3000/ in your browser, login with username "admin" and password "admin",
click "Add data source", fill in a name (e.g. "qcow"), set the type to "Prometheus",
change the URL to "http://localhost:9091", change the type to "direct" and click "Save & Test".
It should say "Success: Data source is working"

Click on the Main menu, hover over "Dashboards" and select "Import". Import the
[dashboard.json](https://raw.githubusercontent.com/mirage/ocaml-qcow/master/doc/dashboard.json).

Once sufficient data has been scraped, the dashboard should look like this:

![screenshot](https://cloud.githubusercontent.com/assets/198586/26151381/7e53db66-3afa-11e7-8608-7ba015c49910.png)
