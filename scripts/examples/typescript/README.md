## Typescript for XenAPI Usage

### Examples
Install lib from npm repository
```
$ npm install xen-api-ts
```

The usage of Typescript XenAPI is almost identical to the [Python XenAPI](https://xapi-project.github.io/xen-api/usage.html), except it's asynchronous and requires async/await.
```
import { xapi_client } from "xen-api-ts"
async function main() {
    const session = xapi_client(process.env.HOST_URL)
    try:
        await session.login_with_password(process.env.USERNAME, process.env.PASSWORD)
        const hosts = await session.xenapi.host.get_all()
    finally:
        await session.xenapi.session.logout()
}
main()
```

For more example usage, we can run as follows.
```
$ cd <examples/typescript dir>
$ npm install
$ echo "HOST_URL=xxx" >> .env
$ echo "USERNAME=xxx" >> .env
$ echo "PASSWORD=xxx" >> .env
$ npm test tests/getXapiVersion.ts
```

## Packaging
