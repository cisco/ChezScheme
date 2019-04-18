/* compress-io.h
 * Copyright 1984-2019 Cisco Systems, Inc.
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

typedef struct glzFile_r {
  INT fd;
  IBOOL inputp;
  INT format;
  union {
    struct gzFile_s *gz;
    struct lz4File_in_r *lz4_in;
    struct lz4File_out_r *lz4_out;
  };
} *glzFile;
