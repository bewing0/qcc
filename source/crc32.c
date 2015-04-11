#include "crc32.h"

uint32_t crc32(uint8_t *p, int len)
{
	uint32_t crc = 0xffffffff;
	while (--len >= 0) crc = poly8_lookup[((uint8_t) crc ^ *(p++))] ^ (crc >> 8);
	return (~crc);
}


uint8_t hash(uint8_t *p, int len)
{
	return (uint8_t) crc32(p, len);
}
