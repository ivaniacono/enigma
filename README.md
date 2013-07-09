EN.I.GM.A.
==========

ErlaNg applIed to a Goal-Manager for Autonomous-systems

ENIGMA is a Goal Manager written in Erlang/OTP and can be used in autonomous
systems like robots. It has the task of executing a set of goals ordered by
priority and feasibility, if one goal is not feasible or its execution fails,
it will be tried again later. Each goal has a priority that can depend on the
enviorment (for example a robot position), also one goal can be feasible or
not in a moment (for example because there is an obstacle).

WARNING: The developing is in progress, so using it for production is not
recomended.


# HOW TO GET IT

Fetch the latest version of ENIGMA using git:

`git clone https://github.com/ivaniacono/enigma.git`


# LICENSE

ENIGMA is licensed under the Apache License, Version 2.0 (the "License");
You may not use this library except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.