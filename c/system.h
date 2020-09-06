/* system.h
 * Copyright 1984-2017 Cisco Systems, Inc.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "scheme.h"
#include "equates.h"
#ifdef FEATURE_WINDOWS
#ifdef __MINGW32__
# undef WINVER
# undef _WIN32_WINNT
#endif
#define WINVER 0x0601 // Windows 7
#define _WIN32_WINNT WINVER
#include <windows.h>
#endif

#include "version.h"
#include <stdio.h>
#include <stddef.h>

#include "thread.h"

#include "types.h"

#include "compress-io.h"

#ifndef EXTERN
#define EXTERN extern
#endif
#include "globals.h"

#include "externs.h"

#include "segment.h"

#include "atomic.h"
