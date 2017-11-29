# Run-B.L.D.

[![Circle CI](https://circleci.com/gh/elastic/runbld/tree/master.svg?style=svg&circle-token=9f37d8182532f6539f497ca287a82a788e9007ef)](https://circleci.com/gh/elastic/runbld/tree/master)

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
% lein package
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

### Java

#### Maven

Install via `apt-get`, `brew`, or whatever package manager your
system uses.  You can also install [manually](https://maven.apache.org/install.html).

#### Elasticsearch

Download the latest Elasticsearch from [here](https://www.elastic.co/downloads/elasticsearch) and start it via
`bin/elasticsearch`.

### Python

#### nose

<http://nose.readthedocs.io/en/latest/>

### go

[Install Go](https://golang.org/doc/install)

#### go-junit-report

<https://github.com/jstemmer/go-junit-report>

(Don't forget to add `$GOPATH/bin` to your path.)

### facter

`facter` is a command written by Puppet to gather data about the
machine on which it is run.  You have a few options to install this
one.

#### Install the Puppet agent

It is packaged with the puppet agent and Puppet supplies
installers and instructions for various operating systems are at
<https://docs.puppet.com/puppet/latest/>

It appears that the `facter` executable will end up in
`/opt/puppetlabs/puppet/bin` or someplace similar.

#### Build and install manually

The other option you have is to build facter by hand.  It requires
2 other Puppet libraries be installed first.  I built specific
tags in order to get a final bin that was reasonably close to what
we need.  Facter's master branch has bumped the version to `4.0.0`
which is untested and not supported in `runbld` at the moment.

-   <https://github.com/puppetlabs/leatherman> - Tag: 0.11.2
-   <https://github.com/puppetlabs/cpp-hocon> - Tag: 0.1.5
-   <https://github.com/puppetlabs/facter> - Tag: 3.6.4

The instructions in the repos worked reasonably well.  I had one
issue with running `make` in the `facter` repo due to some issue
with copying a ruby file to Mac OS X's default Ruby location.  I
failed to figure out why and eventually worked around it by
removing the Ruby deploy directives from the cmake files.

## Running tests

Once all of the dependencies are installed go to the `runbld`
directory and run `make`.  This will run a few sample projects and
produce various Maven formatted XML test output files (some of
which will look like failures, but don't panic). It finishes by
running the Clojure tests, which should print a bunch of output
but ultimately pass.

Future test runs can skip the `make` step and run `lein test`
directly.

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
