import { xapi_client } from "../src/XenAPI"

// Ref https://xapi-project.github.io/xen-api/
async function get_api_version(session: any) {
    const pool = await session.xenapi.pool.get_all()
    const host = await session.xenapi.pool.get_master(pool[0])
    const major = await session.xenapi.host.get_API_version_major(host)
    const minor = await session.xenapi.host.get_API_version_minor(host)
    return `${major}.${minor}`
}

async function main() {
    console.log(`Login ${process.env.HOST_URL} with ${process.env.USERNAME}`)
    const session = xapi_client(process.env.HOST_URL)
    const sid = await session.login_with_password(process.env.USERNAME, process.env.PASSWORD)
    console.log(`Login successfully with ${sid}`)

    const ver = await get_api_version(session)
    console.log(`\nCurrent XAPI Version: ${ver}`)

    const hosts = await session.xenapi.host.get_all()
    console.log(`\nGet Host list:\n${hosts.join("\n")}`)

    await session.xenapi.session.logout()
    console.log(`\nSession Logout.`)
}

main()
