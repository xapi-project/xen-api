#include <stdint.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>

/*
	This code is a C implementation of the SeaHash algorithm (see: http://ticki.github.io/blog/seahash-explained/)
	The author also provides a reference implemenation written in Rust (which follows the MIT license). 
*/

#define BUFF_SIZE (61440)

uint64_t A, B, C, D;

void init() {
	// These default values come from the SeaHash specification
	A = 0x16F11FE89B0D677C,
	B = 0xB480A793D8E6C86C,
	C = 0x6FE2E5AAF078EBC9,
	D = 0x14F994A4C5259381;
}

uint64_t diffuse(uint64_t x) {
	const uint64_t p = 0x6EED0E9DA4D94A4F;
	x *= p;
	x ^= (x >> 32) >> (x >> 60);
	x *= p;
	return x;
}

uint64_t finalise(const uint64_t size) {
	A ^= B;
	C ^= D;
	A ^= C;
	A ^= size;
	return diffuse(A);
}

void rotate(uint64_t x) {
	x = diffuse(A ^ x);
	A = B; // Rotate the lanes
	B = C;
	C = D;
	D = x;
}

void update(const void *data, const uint64_t size) {
	const uint64_t freq = ((size >> 3) >> 2);
	const uint64_t *head = (uint64_t *) data;

	for(uint64_t i = 0; i < freq; i++) {
		// NB: we assume little-endian, as we currently only support x86-64 systems
		A ^= *head++;
		B ^= *head++;
		C ^= *head++;
		D ^= *head++;

		A = diffuse(A);
		B = diffuse(B);
		C = diffuse(C);
		D = diffuse(D);
	}

	const uint8_t remain = size - (size & 0xFFFFFFFFFFFFFFE0); // How many bytes weren't hashed above (n < 32)
	if(remain == 0) return; // Expected case for most common data lengths (e.g. 1MB, 1GB etc.)

	for(uint8_t i = 0; i < (remain >> 3); i++) { // For each 8-byte value that remains
		rotate(*head++);
	}

	const uint8_t rem = (remain % sizeof(uint64_t)); // How many bytes there are left (n < 8)
	if(rem == 0) return;

	const uint8_t *body = (uint8_t *) head;
	uint64_t x = 0;

	// NB: we read backwards because we need little-endian
	for(uint8_t i = 1; i <= rem; i++) {
		x <<= 8;
		x += body[rem - i];
	}

	rotate(x);
}

uint64_t bytes(const void *data, const uint64_t size) {
	init();
	update(data, size);
	return finalise(size);
}

uint64_t file(const char *name) {
	FILE *in = fopen(name, "rb");
	if(in == NULL) {
		fprintf(stderr, "Cannot open file: %s\n", name);
		return 0;		
	}
	init();
	uint8_t *data[BUFF_SIZE];

	uint64_t sum = 0;
	for(;;) {
		const uint64_t size = (uint64_t) fread(data, 1, BUFF_SIZE, in);
		if(size == 0) break; // EOF
		update(data, size);
		sum += size;
	}

	fclose(in);
	return finalise(sum);
}

int main(int argc, char **argv) {
	// These hash outputs were precomputed using the SeaHash reference implementation (which is written in Rust).
	char temp[4096];

	char *a = ""; // 0 bytes
	strcpy(temp, a);
	assert(bytes(a, 0) == 0xC920CA43256FDCB9);

	char *b = "Hello, World!"; // 13 bytes
	strcpy(temp, b);
	assert(bytes(temp, 13) == 0x2EC2572966D006FD);

	char *d = "g0VugDZm43UU4KvVMczhoO0LvDIsAG8F1"; // 33 bytes
	strcpy(temp, d);
	assert(bytes(temp, 33) == 0x709F6C5CF482869E);

	char *f = "gBy7IsF6xgDww7IHAW7u1XCBgqw1NKG2e7rW1kjJ57Om18lF"; // 48 bytes
	strcpy(temp, f);
	assert(bytes(temp, 48) == 0x187E2EE0343CCDC3);

	char *c = "V8lL08akfSYCQpDtKyAH56SQNORwpF4rxXn9H2wDvWKDR5Rn3mdJBftuDSCEZvI"; // 63 bytes
	strcpy(temp, c);
	assert(bytes(temp, 63) == 0x8FDCA544A6B3476F);

	for(size_t i = 0; i < sizeof(temp); i++) temp[i] = i;
	assert(bytes(temp, sizeof(temp)) == 0xE8010714612E6C70);

	file(*argv); 

	puts("Tests Passed!");
}

