for i in packages/*/url; do sed s/git:\\/\\/github.com\\/xen-org\\//\\/repos\\//g $i > $i.tmp; sed s/.git\"$/\"/ $i.tmp > $i; done

