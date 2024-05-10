.PHONY: build clean

build:
	sed -i '/version/s/1.0.0/'"$(XAPI_VERSION)"'/' package.json
	npm install
	npm run build

publish:
	npm publish

clean:
	rm -rf dist/ node-modules/ package-local.json
