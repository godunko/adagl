--
--  Copyright (C) 2018-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body OpenGL.Contexts.Internals is

   ----------------
   -- Is_Current --
   ----------------

   function Is_Current
     (Context : Gdk.GLContext.Gdk_GLContext) return Boolean
   is
      use type Gdk.GLContext.Gdk_GLContext;

   begin
      return Context /= null and then Context = Gdk.GLContext.Get_Current;
   end Is_Current;

   --  function Current_GLFW_Context
   --    return GLFW.GLFWwindow_Access is
   --  begin
   --     if OpenGL.Contexts.Current_Context /= null then
   --        return GLFW_Context (OpenGL.Contexts.Current_Context.all);
   --
   --     else
   --        return null;
   --     end if;
   --  end Current_GLFW_Context;

   --  function GLFW_Context
   --    (Self : OpenGL_Context'Class) return GLFW.GLFWwindow_Access is
   --  begin
   --     return Self.Window;
   --  end GLFW_Context;

end OpenGL.Contexts.Internals;
