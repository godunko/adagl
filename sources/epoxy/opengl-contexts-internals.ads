--
--  Copyright (C) 2018-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package OpenGL.Contexts.Internals is

   function Is_Current (Context : Gdk.GLContext.Gdk_GLContext) return Boolean;
   --  Returns True when given context is not null and current context.

   --  function GLFW_Context
   --   (Self : OpenGL_Context'Class) return GLFW.GLFWwindow_Access;
   --  --  Returns associated GLFW window.

   function Current_Gdk_Context return Gdk.GLContext.Gdk_GLContext
     renames Gdk.GLContext.Get_Current;
   --  Returns current Gdk_GLContext window.

end OpenGL.Contexts.Internals;
