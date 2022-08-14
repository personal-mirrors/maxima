/^@category/ {
    cat=gensub("@category{(.*)}", "\\1", 1)
    ct=cat
    gsub(" ", "-", ct)
    print "@cat2{", cat, ", ", ct, "}"
}

$0 !~ /^@category/ {
    print $0
}
