Assuming you are currently in Terra srouce dir, use the following command to build it:

`{some path}/run.sh /scripts/build-inplace.sh`

It would download some dependencies to `downloads` dir and output the built binaries to the `install`. The space consumption is ~10 GiB.
With the default parallelism of 8, the memory consumption is <10 GiB.
