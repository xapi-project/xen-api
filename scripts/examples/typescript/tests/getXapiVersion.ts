import { xapi_client } from "../src/XenAPI"

async function get_api_version(session: any) {
    const pools = await session.xenapi.pool.get_all_records()
    const master_host_ref = pools[Object.keys(pools)[0]].master
    const host_record = await session.xenapi.host.get_record(master_host_ref)
    return `${host_record.API_version_major}.${host_record.API_version_minor}`
}

async function main() {
    if (process.env.HOST_URL == undefined) {
        console.log("Please set HOST_URL in .env first.")
        process.exit()
    }

    console.log(`Login ${process.env.HOST_URL} with ${process.env.USERNAME}`)
    const session = xapi_client(process.env.HOST_URL)
    try {
        const sid = await session.login_with_password(process.env.USERNAME, process.env.PASSWORD)
        console.log(`Login successfully with ${sid}`)
        const ver = await get_api_version(session)
        console.log(`\nCurrent XAPI Version: ${ver}`)
        const hosts = await session.xenapi.host.get_all()
        console.log(`\nGet Host list:\n${hosts.join("\n")}`)
    } catch (error) {
        console.error("An error occurred:", error)
    } finally {
        await session.xenapi.session.logout()
        console.log(`\nSession Logout.`)
    }
}

main()
