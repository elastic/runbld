# Run-B.L.D.

It wraps!

![Wrappers](https://rockhall.com/media/assets/inductees/default/run_dmc.jpg)

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


## License

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
