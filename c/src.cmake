include_directories(${PROJECT_SOURCE_DIR}/c/)
set(dir ${PROJECT_SOURCE_DIR}/c/)
set(src
  ${src}
  ${dir}/main.c
  ${dir}/statics.c
  ${dir}/segment.c
  ${dir}/alloc.c
  ${dir}/symbol.c
  ${dir}/intern.c
  ${dir}/gcwrapper.c
  ${dir}/gc-oce.c
  ${dir}/gc-ocd.c
  ${dir}/number.c
  ${dir}/schsig.c
  ${dir}/io.c
  ${dir}/new-io.c
  ${dir}/print.c
  ${dir}/fasl.c
  ${dir}/stats.c
  ${dir}/foreign.c
  ${dir}/prim.c
  ${dir}/prim5.c
  ${dir}/flushcache.c
  ${dir}/schlib.c
  ${dir}/thread.c
  ${dir}/expeditor.c
  ${dir}/scheme.c
  ${dir}/windows.c
  )

set(mac_src
  ${mac_src}
  ${dir}/main.c
${dir}/statics.c 
${dir}/segment.c 
${dir}/alloc.c 
${dir}/symbol.c 
${dir}/intern.c 
${dir}/gcwrapper.c 
${dir}/gc-ocd.c 
${dir}/gc-oce.c
${dir}/number.c 
${dir}/schsig.c 
${dir}/io.c 
${dir}/new-io.c 
${dir}/print.c 
${dir}/fasl.c 
${dir}/stats.c 
${dir}/foreign.c 
${dir}/prim.c 
${dir}/prim5.c 
${dir}/flushcache.c
${dir}/schlib.c 
${dir}/thread.c 
${dir}/expeditor.c 
${dir}/scheme.c

${dir}/system.h 
${dir}/types.h 
${dir}/version.h 
${dir}/globals.h 
${dir}/externs.h 
${dir}/segment.h 
${dir}/gc.c 
${dir}/sort.h 
${dir}/thread.h
${dir}/i3le.c
# ${dir}/itest.c 
)


  
