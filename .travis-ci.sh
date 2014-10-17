OPAM_DEPENDS="xapi-idl async async_inotify core message-switch xapi-storage"
case "$OCAML_VERSION,$OPAM_VERSION" in
3.12.1,1.0.0) ppa=avsm/ocaml312+opam10 ;;
3.12.1,1.1.0) ppa=avsm/ocaml312+opam11 ;;
4.00.1,1.0.0) ppa=avsm/ocaml40+opam10 ;;
4.00.1,1.1.0) ppa=avsm/ocaml40+opam11 ;;
4.01.0,1.0.0) ppa=avsm/ocaml41+opam10 ;;
4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam

export OPAMYES=1
#export OPAMVERBOSE=1
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

opam init
# ocamlscript doesn't read .ocamlinit so the ocamlfind opam workaround breaks.
# This is a workaround workaround:
opam install ocamlfind
sudo mkdir -p $(ocamlc -where)
eval `opam config env`
sudo cp $OCAML_TOPLEVEL_PATH/topfind $(ocamlc -where)

opam remote add xapi git://github.com/xapi-project/opam-repo-dev
depext=`opam install -e ubuntu $OPAM_DEPENDS`
sudo apt-get install -qq $depext
opam install ${OPAM_DEPENDS}
eval `opam config env`
make
echo Running make test
make test
