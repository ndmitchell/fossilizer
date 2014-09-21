// Copyright 2011 Google Inc. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

/**
 * @fileoverview Information about the models that open-3d-viewer will display.
 */

/* ====================================================================== */
/*                        ADD YOUR MODELS HERE                            */
/* ====================================================================== */

o3v.MODELS = [
  {
    name: 'fossil1.obj',
    scriptName: 'fossil1.js',
    modelPath: 'models/fossil1/',
    metadataFile: 'entity_metadata.json',
    texturePath: 'models/common/',
    numLayers: 3
  }
  ,
  {
    name: 'Snail_anatomy_hi.obj',
    scriptName: 'Snail_anatomy_hi.js',
    modelPath: 'models/snail_anatomy/',
    metadataFile: 'entity_metadata.json',
    texturePath: 'models/common/',
    numLayers: 4
  }
  ,
  {
    name: 'Bessie.obj',
    scriptName: 'cow_anatomy.js',
    modelPath: 'models/cow_anatomy/',
    metadataFile: 'entity_metadata.json',
    texturePath: 'models/common/',
    numLayers: 5
  }
];
