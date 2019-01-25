BEGIN {
    catboxopen = 0;
    first_item = 1;
}
/(entry|primary) {([^}]+)}/ {
    if (match($0, "{([^}]+)}")) {
	topic = substr($0, RSTART + 1, RLENGTH - 2)
	printf("@c catboxopen = %d\n", catboxopen)
	if (catboxopen) {
	    printf("@closecatbox\n");
	}
	printf("@anchor{Category: %s}\n", topic)
	printf("@opencatbox\n");
	catboxopen = 1
	first_item = 1
	printf("@b{Category: %s}\n\n", topic)
    }
    next
}
/secondary {([^}]+)}/ {
    if (match($0, "{([^}]+)}")) {
	subtopic = substr($0, RSTART + 1, RLENGTH - 2)
	
	if (!first_item) {
	    printf("@html\n&middot;\n@end html\n");
	}
	first_item = 0
	printf("@ref{%s-%s, %s}\n", topic, subtopic, subtopic)
    }
    next
}
{ printf("@c %s\n", $0) }

END {
    print "@closecatbox"
}