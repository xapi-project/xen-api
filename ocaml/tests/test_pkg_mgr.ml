let format_cmd cmd_lines =
  let cmd, args = cmd_lines in
  Printf.sprintf "%s:%s" cmd (String.concat " " args)

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
        ( !Xapi_globs.dnf_cmd
        , [
            "repoquery"
          ; "-a"
          ; "--qf"
          ; "%{name}:|%{epoch}:|%{version}:|%{release}:|%{arch}:|%{repoid}"
          ; "--installed"
          ]
        )
        (Pkg_mgr.Dnf_cmd.repoquery_installed ())
    )
  ]

let test_dnf_clean_all_cache =
  [
    ( "<null>"
    , `Quick
    , check
        (!Xapi_globs.dnf_cmd, ["clean"; "all"])
        (Pkg_mgr.Dnf_cmd.clean_cache ~repo_name:"*")
    )
  ]

let test_dnf_clean_repo_cache =
  [
    ( "<null>"
    , `Quick
    , check
        ( !Xapi_globs.dnf_cmd
        , ["--disablerepo=*"; "--enablerepo=test_repo"; "clean"; "all"]
        )
        (Pkg_mgr.Dnf_cmd.clean_cache ~repo_name:"test_repo")
    )
  ]

let test_dnf_get_pkgs_from_updateinfo =
  let sub_command = "upgrades" in
  let repositories = ["testrepo1"; "testrepo2"] in
  [
    ( "<null>"
    , `Quick
    , check
        ( !Xapi_globs.dnf_cmd
        , [
            "-q"
          ; "--disablerepo=*"
          ; "--enablerepo=testrepo1,testrepo2"
          ; "updateinfo"
          ; "list"
          ; "upgrades"
          ]
        )
        (Pkg_mgr.Dnf_cmd.get_pkgs_from_updateinfo ~sub_command ~repositories)
    )
  ]

let test_dnf_config_repo =
  let config = ["--setopt=testrepo.accesstoken=file:///some/path"] in
  [
    ( "<null>"
    , `Quick
    , check
        ( !Xapi_globs.dnf_cmd
        , [
            "config-manager"
          ; "--setopt=testrepo.accesstoken=file:///some/path"
          ; "testrepo"
          ]
        )
        (Pkg_mgr.Dnf_cmd.config_repo ~repo_name:"testrepo" ~config)
    )
  ]

let test_dnf_sync_repo =
  [
    ( "<null>"
    , `Quick
    , check
        ( !Xapi_globs.dnf_cmd
        , [
            "reposync"
          ; "-p"
          ; !Xapi_globs.local_pool_repo_dir
          ; "--downloadcomps"
          ; "--download-metadata"
          ; "--delete"
          ; "--newest-only"
          ; "--repoid=testrepo"
          ]
        )
        (Pkg_mgr.Dnf_cmd.sync_repo ~repo_name:"testrepo")
    )
  ]

let test_dnf_apply_upgrades =
  [
    ( "<null>"
    , `Quick
    , check
        ( !Xapi_globs.dnf_cmd
        , [
            "-y"
          ; "--disablerepo=*"
          ; "--enablerepo=testrepo1,testrepo2"
          ; "upgrade"
          ]
        )
        (Pkg_mgr.Dnf_cmd.apply_upgrade ~repositories:["testrepo1"; "testrepo2"])
    )
  ]

let test_yum_repo_query_installed =
  [
    ( "<null>"
    , `Quick
    , check
        ( !Xapi_globs.repoquery_cmd
        , [
            "-a"
          ; "--pkgnarrow=installed"
          ; "--qf"
          ; "%{name}:|%{epoch}:|%{version}:|%{release}:|%{arch}:|%{repoid}"
          ]
        )
        (Pkg_mgr.Yum_cmd.repoquery_installed ())
    )
  ]

let test_yum_clean_all_cache =
  [
    ( "<null>"
    , `Quick
    , check
        (!Xapi_globs.yum_cmd, ["clean"; "all"])
        (Pkg_mgr.Yum_cmd.clean_cache ~repo_name:"*")
    )
  ]

let test_yum_clean_repo_cache =
  [
    ( "<null>"
    , `Quick
    , check
        ( !Xapi_globs.yum_cmd
        , ["--disablerepo=*"; "--enablerepo=test_repo"; "clean"; "all"]
        )
        (Pkg_mgr.Yum_cmd.clean_cache ~repo_name:"test_repo")
    )
  ]

let test_yum_get_pkgs_from_updateinfo =
  let sub_command = "updates" in
  let repositories = ["testrepo1"; "testrepo2"] in
  [
    ( "<null>"
    , `Quick
    , check
        ( !Xapi_globs.yum_cmd
        , [
            "-q"
          ; "--disablerepo=*"
          ; "--enablerepo=testrepo1,testrepo2"
          ; "updateinfo"
          ; "list"
          ; "updates"
          ]
        )
        (Pkg_mgr.Yum_cmd.get_pkgs_from_updateinfo ~sub_command ~repositories)
    )
  ]

let test_yum_config_repo =
  let config = ["--setopt=testrepo.accesstoken=file:///some/path"] in
  [
    ( "<null>"
    , `Quick
    , check
        ( !Xapi_globs.yum_config_manager_cmd
        , ["--setopt=testrepo.accesstoken=file:///some/path"; "testrepo"]
        )
        (Pkg_mgr.Yum_cmd.config_repo ~repo_name:"testrepo" ~config)
    )
  ]

let test_yum_sync_repo =
  [
    ( "<null>"
    , `Quick
    , check
        ( !Xapi_globs.reposync_cmd
        , [
            "-p"
          ; !Xapi_globs.local_pool_repo_dir
          ; "--downloadcomps"
          ; "--download-metadata"
          ; "--delete"
          ; "--newest-only"
          ; "--repoid=testrepo"
          ; "--plugins"
          ]
        )
        (Pkg_mgr.Yum_cmd.sync_repo ~repo_name:"testrepo")
    )
  ]

let test_yum_apply_upgrades =
  [
    ( "<null>"
    , `Quick
    , check
        ( !Xapi_globs.yum_cmd
        , [
            "-y"
          ; "--disablerepo=*"
          ; "--enablerepo=testrepo1,testrepo2"
          ; "upgrade"
          ]
        )
        (Pkg_mgr.Yum_cmd.apply_upgrade ~repositories:["testrepo1"; "testrepo2"])
    )
  ]

let tests =
  [
    ("test_dnf_repo_query_installed", test_dnf_repo_query_installed)
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
  ]
