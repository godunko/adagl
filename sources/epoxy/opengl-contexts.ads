--
--  Copyright (C) 2018-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package provides integration with Gtk's GLContext

with Gdk.GLContext;

with OpenGL.Functions;
private with OpenGL.Implementation.libepoxy_Functions;

package OpenGL.Contexts is

   type OpenGL_Context is tagged limited private;

   type OpenGL_Context_Access is access all OpenGL_Context'Class;

   function Create
     (Self      : in out OpenGL_Context'Class;
      GLContext : not null Gdk.GLContext.Gdk_GLContext)
      return Boolean;
   --  Attempts to create the OpenGL context. Returns False on failure.

   procedure Create
     (Self      : in out OpenGL_Context'Class;
      GLContext : not null Gdk.GLContext.Gdk_GLContext);
   --  Attempts to create the OpenGL context. Raise Program_Error on failure.

   function Functions
     (Self : in out OpenGL_Context'Class)
      return access OpenGL.Functions.OpenGL_Functions'Class;
   --  Returns the OpenGL_Functions implementation for this context.

   --  procedure Make_Current (Self : in out OpenGL_Context'Class);
   --  --  Makes the context current.
   --
   --  function Current_Context return OpenGL_Context_Access;
   --  --  Returns the last context which called makeCurrent, or null, if no
   --  --  context is current.

private

   --------------------
   -- OpenGL_Context --
   --------------------

   type OpenGL_Context is tagged limited record
      Functions : aliased
        OpenGL.Implementation.libepoxy_Functions.Epoxy_Functions;
   end record;

end OpenGL.Contexts;
