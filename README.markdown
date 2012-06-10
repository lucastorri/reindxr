# what

`reindxr` recursively search for all documents (pdf, doc, txt, ...) in a same directory, indexes their contents on a language aware index, and make them searchable through a REST API.

Given a dir were all your documents will be placed, let's say `data`, and a dir, `index`, on which the indexed data will be placed, there are 3 different events on `data` that will change the contents of `index`:

* `new file`: a new document is placed in `data`. `reindxr` will notice the added file, load the contents of the document and add them to `index`. As soon as it is indexed, it will then be searchable through the REST API;
* `file modified`: the contents of a document inside `data` that is also in `index` is changed. `reindxr` will detect those changes and update `index` with them;
* `file removed`: a file, previously in `index`, is deleted from the `data` folder. It will then be removed from `index`.



# how

[Apache Lucene](https://lucene.apache.org/) is used to created the indexed data. Language detection and document text extraction is done with [Apache Tika](http://tika.apache.org/).

Once a `new file` or `file modified` event is fired, the text inside that specific document format is extracted and the language used on it is identified. With the language information, the right Lucene analyzer is selected. Currently, there are only two analyzers enabled:
* English
* Brazilian Portuguese

For each enabled language analyzer, a new directory is created in side `index`. For instance, `index/en` and `index/pt`. Documents that match the available analyzers will be placed on one of those `index` subdirectories. Portuguese based languages will fall on the second option, while all the other ones, by default, will be analyzed as English.

The files are identified by their file names, where a file `data/someDocument.pdf` will have id `someDocument.pdf`. For a subdirectory inside `data`, the parent directory name will be used. For instance, `data/someDir/someDocument.pdf` will have id `someDir/someDocument.pdf`. With that, id colision is avoided by the fact that it is not possible to have two files with the same id on `data`.

During a search, all the `index` subdirectories will be searched, using their respective analyzers. For performance purposes, those searches are made in parallel in each subdir. The same happens when deleting: it will traverse all the `index` subdirs. When modified, a deletion will happen first, followed by the reinsertion of the document on `index`.



# rest api

The HTTP server runs on port 8123. There are two ways to access the index:

* Searching: will return all the documents that match the given criteria (based on the Lucene search format) and snippets of the hits:
> /search/#{your search criteria}

* Highlighting: will retuned all the content of a single document, highlighting all the matches of the given criteria:
> /hl/#{the highlighted file}/#{your search criteria}



# usage

* To generate a jar: 
> sbt assembly

* To generate a eclipse project:
> sbt eclipse

* To run 
> java -cp reindxr.jar co.torri.reindxr.Main <dataFolder> <indexFolder> \[serverAddress\] \[ServerPort\]



# other

* You can navigate the created indexes using [Luke](http://code.google.com/p/luke/).