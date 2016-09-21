# Math205-Final-Project
Created By:
Shaun Repp
Trevor Buttrey

Our R script first begins by prompting the user to select a .csv file. After selecting the .csv file the R script places the values of the .csv into arrays for the X values, the Y values, and the Details ie. CSK and ID.
	After processing the .csv, our script checks against each of the conditions in the Robustness document to select appropriate tests to run. Each of these tests, as described in the project description and in the Robustness document are intended to determine if they came from the same population or different populations.
	At each test the script outputs the type of test it is attempting to run followed by the output of the test. If the test cannot be run because the robustness conditions were not met, the script will output which condition caused it to fail. Notice that it only outputs the first condition that isn’t met, based on the order of execution. Since every condition must be met we decided that having only the first failure condition print out was sufficient.
	This process of 1) robustness 2) test/failure output continues for each of the different tests as described in the Robustness document. 
	We only run each of the tests when the data type is appropriate. These are the data types we accept for each of the tests:
Center for Paired t-Test
Count for Sign Test
Center for Two Sample t-Test (Non Pooled)
Center for Two Sample t-Test (Pooled)
Spread for F Test
Count for Two Proportion Test
If the data type is incorrect the test will not run.
We aimed for our script to meet the project requirements while providing the user with useful output to describe what is occurring during each step of execution. This extra output was also useful to us during writing and debugging.
