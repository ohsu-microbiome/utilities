# Project Utilities

Scripts for initializing project and data folders.

Currently no help docs. Need to look at code.

## Initialize basic microbiome project

```
balter@exanode-3-2:~/teamMB/utilities$ initialize_microbiome_project proj_name
balter@exanode-3-2:~/teamMB/utilities$ tree proj_name/
proj_name/
├── Readme.md
├── results
└── src
```

## Initialize documentation for sequencing data

```
balter@exanode-3-2:~/teamMB/utilities$ initialize_sequence_file seq_name
balter@exanode-3-2:~/teamMB/utilities$ tree seq_name/
seq_name/
├── fastq
├── processed
│   ├── params.yml
│   └── README.md
├── QC
└── README.md
```

## `.gitignore`

Copy the `gitignore` file to your project file with a leading period.

```
cp gitignore <project-path>/.gitignore
```
