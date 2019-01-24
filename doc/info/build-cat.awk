BEGIN {
    catboxopen = 0;
    first_item = 1;
}
{
    if (match($0, "(entry|primary) {([^}]+)}", result)) {
	topic = result[2]
	printf("@c catboxopen = %d\n", catboxopen)
	if (catboxopen) {
	    printf("@closecatbox\n");
	}
	printf("@anchor{Category: %s}\n", topic)
	printf("@opencatbox\n");
	catboxopen = 1
	first_item = 1
	printf("@b{Category: %s}\n\n", topic)
    } else if (match($0, "secondary {([^}]+)}", subtopic)) {
	if (!first_item) {
	    printf("@html\n&middot;\n@end html\n");
	}
	first_item = 0
	printf("@ref{%s-%s, %s}\n", topic, subtopic[1], subtopic[1])
    }
}

END {
    print "@closecatbox"
}