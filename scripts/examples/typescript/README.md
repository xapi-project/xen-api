# A Typescript bindings for XenAPI
A Typescript bindings for XenAPI, which is inspired by [Python XenAPI](https://xapi-project.github.io/xen-api/usage.html).

## Usage
Install lib from npm repository
```
$ npm install xen-api-ts
```

The usage of Typescript XenAPI is almost identical to the Python XenAPI, except it's asynchronous and requires async/await.
```
import { xapi_client } from "xen-api-ts";

async function main() {
  const session = xapi_client(process.env.HOST_URL);
  try {
    await session.login_with_password(process.env.USERNAME, process.env.PASSWORD);
    const hosts = await session.xenapi.host.get_all();
    console.log(hosts); // Do something with the retrieved hosts
  } finally {
    await session.xenapi.session.logout();
  }
}

main();
```

For more example usage, please find in tests folder.
```
$ git clone git@github.com:acefei/xen-api-ts.git
$ npm install
$ echo "HOST_URL=xxx" >> .env
$ echo "USERNAME=xxx" >> .env
$ echo "PASSWORD=xxx" >> .env
$ npm test tests/getXapiVersion.ts
```

And please find the all of Classes and its Fields in [XenAPI Reference](https://xapi-project.github.io/xen-api)

## License
This repository is licensed under the MIT License.
