
**Notice 2018/09/25: Runbld has been incorporated into some internal tooling and is no longer maintained as OSS.**

----

# Run-B.L.D.

It wraps!

[![Other Wrappers](https://upload.wikimedia.org/wikipedia/en/1/1f/Rundmc_2.jpg)](https://en.wikipedia.org/wiki/Run%E2%80%93D.M.C.)

When you need the power of Jenkins, but the sanity of change
management.

## Usage

Single execution model supported for now, primarily to support
Jenkins's ssh-slaves plugin and default shell build step:

```
runbld /path/to/script.bash
```

## Install

```
% make package
```

Creates a Java binary in `target/` that you can run on OS X, Linux,
and Windows. Distribute to your build slaves as needed. Then, create a
build step in your Jenkins job:

```
#!/path/to/runbld
echo now some bash goes here
echo it does have to be bash for now
exit 0
```

## Example

```
% cat fail.bash 
echo this is gonna fail
exit 1
% runbld ./fail.bash
>>>>>>>>>>>> SCRIPT EXECUTION BEGIN >>>>>>>>>>>>
+ echo this is gonna fail
this is gonna fail
+ exit 1
<<<<<<<<<<<< SCRIPT EXECUTION END   <<<<<<<<<<<<
DURATION: 9ms
WRAPPER: FAILURE (1)
% echo $?
0
%
```


## Development

To see all of the tests passing there are several dependencies that
need to be installed.

### Elasticsearch

Download the latest Elasticsearch from [here](https://www.elastic.co/downloads/elasticsearch) and start it via
`bin/elasticsearch`.


## Running tests

`lein test` to run the tests and `lein tests!` to clean and test.


## Building the Junit test output

The files are checked in, but if there is ever a need to rebuild them,
some extra dependencies are necessary.  After they are installed go to
the `runbld` directory and run `make junit`.  This will run a few
sample projects and produce various Maven formatted XML test output
files (some of which will look like failures, but don't panic).

### Java

#### Maven

Install via `apt-get`, `brew`, or whatever package manager your
system uses.  You can also install [manually](https://maven.apache.org/install.html).

### Python

#### nose

<http://nose.readthedocs.io/en/latest/>

### go

[Install Go](https://golang.org/doc/install)

#### go-junit-report

<https://github.com/jstemmer/go-junit-report>

(Don't forget to add `$GOPATH/bin` to your path.)


# License

```
This software is licensed under the Apache License, version 2 ("ALv2"), quoted below.

Copyright 2009-2015 Elasticsearch <https://www.elastic.co>

Licensed under the Apache License, Version 2.0 (the "License"); you may not
use this file except in compliance with the License. You may obtain a copy of
the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
License for the specific language governing permissions and limitations under
the License.
```
