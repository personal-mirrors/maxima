This is a try to write a script that sets up a lightweight virtual
machine (a container) that builds debian packages of maxima on
an old system, but using the kernel that is running on the current
computer.

The rationale behind that is that really old systems don't have
dependencies on libraries and library versions part of our users
don't have.

Just run

build.sh

It should be intelligent enough to automatically download the
most current Maxima release and if the ChangeLog in the
DebianPackaging branch of the maxima Repo matches this version
number everything should work out-of-the box.
