--
--  Copyright (C) 2018-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Interfaces.C.Strings;
with System.Storage_Elements;
with epoxy_gl_generated_h;

package body OpenGL.Contexts is

   Current : OpenGL_Context_Access;

   ------------
   -- Create --
   ------------

   function Create
     (Self      : in out OpenGL_Context'Class;
      GLContext : not null Gdk.GLContext.Gdk_GLContext) return Boolean
   is
      --  use GLFW;
      --
      --  function glewInit return Interfaces.C.unsigned
      --    with Import, Convention => C, External_Name => "glewInit";
      --
      --  name : Interfaces.C.char_array := Interfaces.C.To_C ("Aaa");
      --  ko   : Interfaces.C.int;
      --  ok   : Interfaces.C.unsigned;
   begin
      --  ko := glfwInit;
      --  pragma Assert (ko in 1);
      --  Self.Window := glfwCreateWindow (640, 480, name, null, null);
      --  Self.Make_Current;
      --  ok := glewInit;
      --  pragma Assert (ok in 0);

      return True;
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (Self      : in out OpenGL_Context'Class;
      GLContext : not null Gdk.GLContext.Gdk_GLContext) is
   begin
      if not Self.Create (GLContext) then
         raise Program_Error with "Create fails";
      end if;
   end Create;

   ---------------
   -- Functions --
   ---------------

   function Functions
     (Self : in out OpenGL_Context'Class)
      return access OpenGL.Functions.OpenGL_Functions'Class
   is
   begin
      return Self.Functions'Unchecked_Access;
   end Functions;

   ------------------
   -- Make_Current --
   ------------------

   --  procedure Make_Current (Self : in out OpenGL_Context'Class) is
   --  begin
   --     Current := Self'Unchecked_Access;
   --     GLFW.glfwMakeContextCurrent (Self.Window);
   --  end Make_Current;

   ---------------------
   -- Current_Context --
   ---------------------

   --  function Current_Context return OpenGL_Context_Access is
   --  begin
   --     return Current;
   --  end Current_Context;

end OpenGL.Contexts;
