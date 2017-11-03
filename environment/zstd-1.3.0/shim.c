#include <stdlib.h>
#include <string.h>
#include "lib/zstd.h"

ZSTD_inBuffer* ZSTDshim_makeInBuffer(void* buffIn, size_t size) {
  ZSTD_inBuffer* input = malloc(sizeof(ZSTD_inBuffer));
  input->src = buffIn;
  input->size = size;
  input->pos = 0;
  return input;
}

int ZSTDshim_inBufferExhausted(ZSTD_inBuffer *buffer) {
  return buffer->pos >= buffer->size;
}

ZSTD_outBuffer* ZSTDshim_makeOutBuffer(void* buffIn, size_t size) {
  ZSTD_outBuffer* output = malloc(sizeof(ZSTD_outBuffer));
  output->dst = buffIn;
  output->size = size;
  output->pos = 0;
  return output;
}

size_t ZSTDshim_outBufferPos(ZSTD_outBuffer *buffer) {
  return buffer->pos;
}

char* ZSTDshim_decompress(void* buff, size_t size, size_t* outputSize) {
  void* buffIn = malloc(ZSTD_DStreamInSize());
  void* buffOut = malloc(ZSTD_DStreamOutSize());
  size_t pos = 0;

  ZSTD_DStream* const dstream = ZSTD_createDStream();
  if (dstream == NULL) {
    return NULL;
  }

  size_t const initResult = ZSTD_initDStream(dstream);
  if (ZSTD_isError(initResult)) {
    return NULL;
  }

  size_t toRead = initResult;
  char* result = NULL;
  size_t resultSize = 0;

  while (pos < size) {
    size_t bytesRead = toRead;
    if (size - pos < toRead) {
      bytesRead = size - pos;
    }
    memcpy(buffIn, buff + pos, bytesRead);
    pos += bytesRead;
    ZSTD_inBuffer input = { buffIn, bytesRead, 0 };
    while (input.pos < input.size) {
      ZSTD_outBuffer output = { buffOut, ZSTD_DStreamOutSize(), 0 };
      toRead = ZSTD_decompressStream(dstream, &output, &input);
      if (ZSTD_isError(toRead)) {
        return NULL;
      }

      result = realloc(result, resultSize + output.pos);
      memmove(result + resultSize, buffOut, output.pos);
      resultSize += output.pos;
    }
  }

  ZSTD_freeDStream(dstream);
  free(buffIn);
  free(buffOut);

  *outputSize = resultSize;
  return result;
}
