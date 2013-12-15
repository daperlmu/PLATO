In order to run the regression tests, simply do the following:
	./regress.sh dir_to_test_files
	where "dir_to_test_files" is the directory where the *.plt test files are located.
	
	For example:
	./regress.sh samplePltFiles
	Ensure, that for every *.plt file, there is a *.result file with the same name, which contains the expected output as a result of running the *.plt file.
	For instance, if you have a file called printStatementTest.plt, you should also have a printStatementTest.result file.

In order to simply clean compile a *.plt file, to test whether it compiles successfully, simply do:
	./plt.sh path_to_plt_file clean no_run

In order to clean compile a *.plt file and run it, simply do:
	./plt.sh path_to_plt_file clean run

In order to dirty compile a *.plt file, to test whether it compiles successfully, simply do:
	./plt.sh path_to_plt_file dirty no_run

In order to dirty compile a *.plt file and run it, simply do:
	./plt.sh path_to_plt_file dirty run