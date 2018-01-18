------------------------------------------------------------------------------
--                                                                          --
--                       Ada binding for OpenGL/WebGL                       --
--                                                                          --
--                        Runtime Library Component                         --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2018, Vadim Godunko <vgodunko@gmail.com>                     --
-- All rights reserved.                                                     --
--                                                                          --
-- Redistribution and use in source and binary forms, with or without       --
-- modification, are permitted provided that the following conditions       --
-- are met:                                                                 --
--                                                                          --
--  * Redistributions of source code must retain the above copyright        --
--    notice, this list of conditions and the following disclaimer.         --
--                                                                          --
--  * Redistributions in binary form must reproduce the above copyright     --
--    notice, this list of conditions and the following disclaimer in the   --
--    documentation and/or other materials provided with the distribution.  --
--                                                                          --
--  * Neither the name of the Vadim Godunko, IE nor the names of its        --
--    contributors may be used to endorse or promote products derived from  --
--    this software without specific prior written permission.              --
--                                                                          --
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      --
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        --
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR    --
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT     --
-- HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED --
-- TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR   --
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF   --
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     --
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS       --
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.             --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C;

package GLFW is

   pragma Preelaborate;

   type GLFWwindow is null record with Convention => C;
   type GLFWwindow_Access is access all GLFWwindow;

   function glfwInit return Interfaces.C.int
     with Import, Convention => C, External_Name => "glfwInit";

   function glfwCreateWindow
     (width, height : Interfaces.C.int;
      title : Interfaces.C.char_array;
      monitor : GLFWwindow_Access;
      share   : GLFWwindow_Access) return GLFWwindow_Access
     with Import, Convention => C, External_Name => "glfwCreateWindow";

   procedure glfwMakeContextCurrent (window : GLFWwindow_Access)
     with Import, Convention => C,
     External_Name => "glfwMakeContextCurrent";

   function glfwWindowShouldClose (window : GLFWwindow_Access)
                                      return Interfaces.C.int
     with Import, Convention => C,
     External_Name => "glfwWindowShouldClose";

   procedure glfwPollEvents
     with Import, Convention => C, External_Name => "glfwPollEvents";

   procedure glfwWaitEvents
     with Import, Convention => C, External_Name => "glfwWaitEvents";

   procedure glfwSwapBuffers (window : GLFWwindow_Access)
     with Import, Convention => C, External_Name => "glfwSwapBuffers";

end GLFW;
