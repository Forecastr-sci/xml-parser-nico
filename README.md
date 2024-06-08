# xml-parser-nico
A simple project to get dataframe of signal countings from CellCounter_Marker_File XML file

## Quickstart
Make sure you have RScript installed on your computer. To verify it, run the following command:
```shell
Rscript --version
```

Help menu of CLI app is called by appending `-h|--help` flag:

To use the app you must provide a name or absolute path to one of the following:
* `.xml` file with valid structure of CellCounter_Marker_File (not check yet, program will just crash)
* `.txt` or `.list` file with each line containing a name or absolute path to the `.xml` file.

Recursive search is allowed, meaning your `.txt` or `.list` file may contain other `.txt` or `.list` files, just make sure to include `.xml` files somewhere.

NOTE: extensions are mandatory for now, in one of the future updates I will try to check for those as well.

If the program cannot find valid XML on the path it will make a short pause to give you opportunity to halt the program and fix typos. This is done, because when launched in terminal, programs written in R don't wait for a response to the input, which makes input validation quite difficult. Same will happen when you try to overwrite existing `.RData` file.

### Flags
`-i/--input` - allows to append the data obtained in current parsing to existing `.RData` file. Just make sure that the countings have the same structure
`-o/--output` - allows to specify name of resulting RData file. When writing both input and output files, extension is mandatory! If -o is missing, filename defaults to timestamp+dp (thinking about including the `Image_Filename` from initial XML).

# Final words
Please write issues on Github if there are problems, this is my first real-life project, and I'm looking forward to making it more advanced!