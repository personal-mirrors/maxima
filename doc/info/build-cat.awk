BEGIN {
    catboxopen = 0;
}
{
    if (match($0, "entry {([^}]+)}", result)) {
	topic = result[1]
	printf("@c catboxopen = %d\n", catboxopen)
	if (catboxopen) {
	    printf("@closecatbox\n");
	}
	printf("@anchor{Category: %s}\n", topic)
	printf("@opencatbox\n");
	catboxopen = 1
	printf("@b{Category: %s}\n\n", topic)
    } else if (match($0, "secondary {([^}]+)}", subtopic)) {
	printf("@ref{%s-%s, %s}\n", topic, subtopic[1], subtopic[1])
	printf("@html\n&middot;\n@end html\n");
    }
}

END {
    print "@closecatbox"
}