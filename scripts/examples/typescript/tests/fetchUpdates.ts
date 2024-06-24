import { xapi_client } from "../src/XenAPI"
import axios from 'axios'

async function get_pool_ref(session: any) {
    const pool = await session.xenapi.pool.get_all()
    return pool[0]
}

async function set_pool_repos(session: any, pool_ref:string, repo_refs: string[]) {
    return await session.xenapi.pool.set_repositories(pool_ref, repo_refs)
}

async function create_repos(session: any) {
    // Ref http://xapi-project.github.io/xen-api/classes/repository.html
	const repositories: { [key: string]: any } = {
		base: {
			name_label: 'base',
			name_description: 'base rpm repo',
			binary_url: 'https://repo.ops.xenserver.com/xs8/base',
			source_url: 'https://repo-src.ops.xenserver.com/xs8/base',
			// base repo is not an update repo
			update: false,
		},
		normal: {
			name_label: 'normal',
			name_description: 'normal rpm repo',
			binary_url: 'https://repo.ops.xenserver.com/xs8/normal',
			source_url: 'https://repo-src.ops.xenserver.com/xs8/normal',
			update: true,
		},
	}

    const res = Object.keys(repositories).sort().map(async repoKey => {
        const repo = repositories[repoKey]
        const { name_label, name_description, binary_url, source_url, update  } = repo
        return await session.xenapi.Repository.introduce(name_label, name_description, binary_url, source_url, update)
    })

    const resPromises = await Promise.all(res)
    return resPromises
}

async function get_repos(session: any) {
    return await session.xenapi.Repository.get_all()
}

async function remove_repos(session: any) {
    const repo_refs = await get_repos(session)
    const res = repo_refs.map(async (ref:string)=> {
        return await session.xenapi.Repository.forget(ref)
    })
    await Promise.all(res)
}

async function sync_updates(session: any, pool_ref:string) {
    return await session.xenapi.pool.sync_updates(pool_ref, false, '', '')
}

async function list_updates(session: any, session_id:string) {
    try {
        const response = await axios.get(`${process.env.HOST_URL}/updates`, {
            params: { 
                session_id: session_id
            },
            httpsAgent: {
                rejectUnauthorized: false
            }
        })
        console.log('Response Data:', response.data)
        return response.data
    } catch (error) {
        console.error('Error:', error)
    }
}

async function main() {
    console.log(`Login ${process.env.HOST_URL} with ${process.env.USERNAME}`)
    const session = xapi_client(process.env.HOST_URL)
    const sid = await session.login_with_password(process.env.USERNAME, process.env.PASSWORD)
    console.log(`Login successfully with ${sid}`)

    const pool_ref = await get_pool_ref(session)
    let repo_refs = await get_repos(session)
    if (!repo_refs.length) {
        console.log('\nCreate new rpm repos')
        repo_refs = await create_repos(session)
    }

    console.log('\nSet enabled set of repositories',repo_refs) 
    await set_pool_repos(session, pool_ref, repo_refs)

    console.log('\nSync updates')
    await sync_updates(session, pool_ref)

    console.log('\nList updates')
    const updates = await list_updates(session, sid)
    console.log(updates)

    console.log('\nClean old rpm repos')
    await set_pool_repos(session, pool_ref, [])
    await remove_repos(session)

    await session.xenapi.session.logout()
    console.log(`\nSession Logout.`)
}

main()
