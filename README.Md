Sometimes you find out you have replicate individuals/technical replicates in your structure file. This can bias structure's ability to find the underlying number of clusters because it distorts allele frequencies, so you want to remove/combine replicates of the same individuals.

Ideally you'd go back to whatever data you used to generate the structure file in the first place, but if for some reason that is not possible (e.g. https://github.com/dereneaton/ipyrad/issues/289), then you might need a utility for combining your reps at the "final structure" file stage.

combining_samples_from_structure.R will do this for you

Alternately, you may have identified pairs or groups of related individuals (potentially using https://github.com/laninsky/SNP_comparisons) and now want to exclude all but one of these pairs/groups in order to avoid distorting allele frequencies. remove_lowest_coverage_sample_from_group.R will do this for you based on a structure file, and a tab-separated file that gives the pairs of related individuals (in a pairwise fashion e.g. if you have a group of three related individuals, the program will chose between the first two individuals first, and then between the "winning individual" of those first two and the third individual).
```
# Example of related individuals file
sample1	sample2
3.4	SP30
3.4	AM28
3.4	9.Lpry
3.4	7.1
3.4	AM13
```
