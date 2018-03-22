Sometimes you find out you have replicate individuals/technical replicates in your structure file. This can bias structure's ability to find the underlying number of clusters because it distorts allele frequencies, so you want to remove/combine replicates of the same individuals.

Ideally you'd go back to whatever data you used to generate the structure file in the first place, but if for some reason that is not possible (e.g. https://github.com/dereneaton/ipyrad/issues/289), then you might need a utility for combining your reps at the "final structure" file stage.


