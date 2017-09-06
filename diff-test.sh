#!/bin/bash

diff test-src/sample.c test-gen/sample.c &&
diff test-src/sample.h test-gen/sample.h &&
diff test-src/sample_internal_0.c test-gen/sample_internal_0.c
