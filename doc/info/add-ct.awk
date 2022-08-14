/^@node/ {
    target=gensub("(@node) *([^,]+) *,.*", "\\2", 1)
    name=target
}

/^@deffn / {
    target=gensub("(@deffn +{[^}]+}) *([^ ]+).*", "\\2", 1)
    name=target
}
/^@defvr / {
    target=gensub("(@defvr +{.*}) *(.*)", "\\2", 1)
    name=target
}
/^@category/ {
    target=gensub("@category{(.*)}", "\\1", 1)
    print "@ctindex ", target, " @subentry ", name
}
{
    print $0
}

    
