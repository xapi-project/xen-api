open Pkg_mgr

let format_cmd cmd_line' =
  let {cmd; params} = cmd_line' in
  Printf.sprintf "%s:%s" cmd (String.concat " " params)

let check exp ac () =
  let exp_str = format_cmd exp in
  let ac_str = ac |> format_cmd in
  let msg = Printf.sprintf "%s -- %s" exp_str ac_str in
  Alcotest.(check string) msg exp_str ac_str

let test_dnf_repo_query_installed =
  [
    ( "<null>"
    , `Quick
    , check
        {
          cmd= !Xapi_globs.dnf_cmd
        ; params=
            [
              "repoquery"
            ; "--qf"
            ; "%{name}:|%{epoch}:|%{version}:|%{release}:|%{arch}:|%{repoid}\n"
            ; "--installed"
            ]
        }
        (Pkg_mgr.Dnf_cmd.repoquery_installed ())
    )
  ]

let test_dnf_repo_query_updates =
  [
    ( "<null>"
    , `Quick
    , check
        {
          cmd= !Xapi_globs.dnf_cmd
        ; params=
            [
              "repoquery"
            ; "--disablerepo=*"
            ; "--enablerepo=testrepo1,testrepo2"
            ; "--qf"
            ; "%{name}:|%{epoch}:|%{version}:|%{release}:|%{arch}:|%{repoid}\n"
            ; "--upgrades"
            ]
        }
        (Pkg_mgr.Dnf_cmd.repoquery_updates
           ~repositories:["testrepo1"; "testrepo2"]
        )
    )
  ]

let test_dnf_clean_all_cache =
  [
    ( "<null>"
    , `Quick
    , check
        {cmd= !Xapi_globs.dnf_cmd; params= ["clean"; "all"]}
        (Pkg_mgr.Dnf_cmd.clean_cache ~repo_name:"*")
    )
  ]

let test_dnf_clean_repo_cache =
  [
    ( "<null>"
    , `Quick
    , check
        {
          cmd= !Xapi_globs.dnf_cmd
        ; params= ["--disablerepo=*"; "--enablerepo=test_repo"; "clean"; "all"]
        }
        (Pkg_mgr.Dnf_cmd.clean_cache ~repo_name:"test_repo")
    )
  ]

let test_dnf_get_pkgs_from_updateinfo =
  let repositories = ["testrepo1"; "testrepo2"] in
  [
    ( "<null>"
    , `Quick
    , check
        {
          cmd= !Xapi_globs.dnf_cmd
        ; params=
            [
              "-q"
            ; "--disablerepo=*"
            ; "--enablerepo=testrepo1,testrepo2"
            ; "advisory"
            ; "list"
            ; "--updates"
            ]
        }
        (Pkg_mgr.Dnf_cmd.get_pkgs_from_updateinfo Pkg_mgr.Updateinfo.Updates
           ~repositories
        )
    )
  ]

let test_dnf_config_repo =
  let config = ["accesstoken=file:///some/path"] in
  [
    ( "<null>"
    , `Quick
    , check
        {
          cmd= !Xapi_globs.dnf_cmd
        ; params=
            [
              "config-manager"
            ; "setopt"
            ; "testrepo.accesstoken=file:///some/path"
            ]
        }
        (Pkg_mgr.Dnf_cmd.config_repo ~repo_name:"testrepo" ~config)
    )
  ]

let test_dnf_sync_repo =
  [
    ( "<null>"
    , `Quick
    , check
        {
          cmd= !Xapi_globs.dnf_cmd
        ; params=
            [
              "reposync"
            ; "-p"
            ; !Xapi_globs.local_pool_repo_dir
            ; "--download-metadata"
            ; "--delete"
            ; "--newest-only"
            ; "--repoid=testrepo"
            ]
        }
        (Pkg_mgr.Dnf_cmd.sync_repo ~repo_name:"testrepo")
    )
  ]

let test_dnf_apply_upgrades =
  [
    ( "<null>"
    , `Quick
    , check
        {
          cmd= !Xapi_globs.dnf_cmd
        ; params=
            [
              "-y"
            ; "--disablerepo=*"
            ; "--enablerepo=testrepo1,testrepo2"
            ; "upgrade"
            ]
        }
        (Pkg_mgr.Dnf_cmd.apply_upgrade ~repositories:["testrepo1"; "testrepo2"])
    )
  ]

let test_yum_repo_query_installed =
  [
    ( "<null>"
    , `Quick
    , check
        {
          cmd= !Xapi_globs.repoquery_cmd
        ; params=
            [
              "--all"
            ; "--pkgnarrow=installed"
            ; "--qf"
            ; "%{name}:|%{epoch}:|%{version}:|%{release}:|%{arch}:|%{repoid}"
            ]
        }
        (Pkg_mgr.Yum_cmd.repoquery_installed ())
    )
  ]

let test_yum_clean_all_cache =
  [
    ( "<null>"
    , `Quick
    , check
        {cmd= !Xapi_globs.yum_cmd; params= ["clean"; "all"]}
        (Pkg_mgr.Yum_cmd.clean_cache ~repo_name:"*")
    )
  ]

let test_yum_clean_repo_cache =
  [
    ( "<null>"
    , `Quick
    , check
        {
          cmd= !Xapi_globs.yum_cmd
        ; params= ["--disablerepo=*"; "--enablerepo=test_repo"; "clean"; "all"]
        }
        (Pkg_mgr.Yum_cmd.clean_cache ~repo_name:"test_repo")
    )
  ]

let test_yum_get_pkgs_from_updateinfo =
  let repositories = ["testrepo1"; "testrepo2"] in
  [
    ( "<null>"
    , `Quick
    , check
        {
          cmd= !Xapi_globs.yum_cmd
        ; params=
            [
              "-q"
            ; "--disablerepo=*"
            ; "--enablerepo=testrepo1,testrepo2"
            ; "updateinfo"
            ; "list"
            ; "updates"
            ]
        }
        (Pkg_mgr.Yum_cmd.get_pkgs_from_updateinfo Pkg_mgr.Updateinfo.Updates
           ~repositories
        )
    )
  ]

let test_yum_config_repo =
  let config = ["accesstoken=file:///some/path"] in
  [
    ( "<null>"
    , `Quick
    , check
        {
          cmd= !Xapi_globs.yum_config_manager_cmd
        ; params=
            [
              "--save"
            ; "--setopt=testrepo.accesstoken=file:///some/path"
            ; "testrepo"
            ]
        }
        (Pkg_mgr.Yum_cmd.config_repo ~repo_name:"testrepo" ~config)
    )
  ]

let test_yum_sync_repo =
  [
    ( "<null>"
    , `Quick
    , check
        {
          cmd= !Xapi_globs.reposync_cmd
        ; params=
            [
              "-p"
            ; !Xapi_globs.local_pool_repo_dir
            ; "--download-metadata"
            ; "--delete"
            ; "--newest-only"
            ; "--repoid=testrepo"
            ; "--downloadcomps"
            ; "--plugins"
            ]
        }
        (Pkg_mgr.Yum_cmd.sync_repo ~repo_name:"testrepo")
    )
  ]

let test_yum_apply_upgrades =
  [
    ( "<null>"
    , `Quick
    , check
        {
          cmd= !Xapi_globs.yum_cmd
        ; params=
            [
              "-y"
            ; "--disablerepo=*"
            ; "--enablerepo=testrepo1,testrepo2"
            ; "upgrade"
            ]
        }
        (Pkg_mgr.Yum_cmd.apply_upgrade ~repositories:["testrepo1"; "testrepo2"])
    )
  ]

let test_yum_repo_query_updates =
  [
    ( "<null>"
    , `Quick
    , check
        {
          cmd= !Xapi_globs.repoquery_cmd
        ; params=
            [
              "--disablerepo=*"
            ; "--enablerepo=testrepo1,testrepo2"
            ; "--qf"
            ; "%{name}:|%{epoch}:|%{version}:|%{release}:|%{arch}:|%{repoid}"
            ; "--all"
            ; "--pkgnarrow updates"
            ; "--plugins"
            ]
        }
        (Pkg_mgr.Yum_cmd.repoquery_updates
           ~repositories:["testrepo1"; "testrepo2"]
        )
    )
  ]

let tests =
  [
    ("test_dnf_repo_query_installed", test_dnf_repo_query_installed)
  ; ("test_dnf_repo_query_updates", test_dnf_repo_query_updates)
  ; ("test_dnf_clean_all_cache", test_dnf_clean_all_cache)
  ; ("test_dnf_clean_repo_cache", test_dnf_clean_repo_cache)
  ; ("test_dnf_get_pkgs_from_updateinfo", test_dnf_get_pkgs_from_updateinfo)
  ; ("test_dnf_cofig_repo", test_dnf_config_repo)
  ; ("test_dnf_sync_repo", test_dnf_sync_repo)
  ; ("test_dnf_apply_upgrades", test_dnf_apply_upgrades)
  ; ("test_yum_repo_query_installed", test_yum_repo_query_installed)
  ; ("test_yum_clean_all_cache", test_yum_clean_all_cache)
  ; ("test_yum_clean_repo_cache", test_yum_clean_repo_cache)
  ; ("test_yum_get_pkgs_from_updateinfo", test_yum_get_pkgs_from_updateinfo)
  ; ("test_yum_cofig_repo", test_yum_config_repo)
  ; ("test_yum_sync_repo", test_yum_sync_repo)
  ; ("test_yum_apply_upgrades", test_yum_apply_upgrades)
  ; ("test_yum_repo_query_updates", test_yum_repo_query_updates)
  ]

let () = Alcotest.run "Pkg_mgr suite" tests
