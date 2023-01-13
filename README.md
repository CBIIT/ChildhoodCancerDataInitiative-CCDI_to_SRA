# ChildhoodCancerDataInitiative-CCDI_to_SRA
This script will take data from a validated CCDI submission manifest, and create an SRA submission file specifically for a CCDI project.

To run the script on a complete and validated [CCDI validated submission template](https://github.com/CBIIT/ccdi-model/tree/main/metadata-manifest), run the following command in a terminal where R is installed for help.


```
Rscript --vanilla CCDI_to_SRA.R -h
```

```
Usage: CCDI_to_SRA.R [options]

CCDI_to_SRA v2.0.0

Options:
	-f CHARACTER, --file=CHARACTER
		A validated dataset file  based on the template CCDI_submission_metadata_template (.xlsx)

	-s CHARACTER, --previous_submission=CHARACTER
		A previous SRA submission file (xlsx) from the same phs_id study.

	-t CHARACTER, --template=CHARACTER
		A dbGaP SRA metadata template, 'phsXXXXXX'

	-h, --help
		Show this help message and exit

```

An example usage would be the following:

```
Rscript --vanilla CCDI_to_SRA.R -f test_files/a_all_pass_CCDI_Submission_Template_v1.0.2.xlsx -t phsXXXXXX.xlsx 
The SRA submission file is being made at this time.


Process Complete.

The output file can be found here: ChildhoodCancerDataInitiative-CCDI_to_SRA/test_files/_SRA_submission_20230103/
```

If there are any errors or problems with the file, it will write out the possible issues. Most of these should not occur as these submission manifests should be valid based on the [CCDI-Submission_ValidatoR](https://github.com/CBIIT/ChildhoodCancerDataInitiative-Submission_ValidatoR).

For concatenation of a new SRA submission template onto an older submission template, the code would be:

```
Rscript --vanilla CCDI_to_SRA.R -f test_files/a_all_pass_CCDI_Submission_Template_v1.0.2.xlsx -t phsXXXXXX.xlsx -s test_files/b_add_on_SRA_submission.xlsx 
The SRA submission file is being made at this time.


Process Complete.

The output file can be found here: ChildhoodCancerDataInitiative-CCDI_to_SRA/test_files/phs987654_SRA_submission_20230103/
```
