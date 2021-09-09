/*
* @Author: Ruige Lee
* @Date:   2021-09-01 16:37:44
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-09-02 11:14:20
*/

#include <sys/mman.h>

#define EMU_RAM_SIZE (8 * 1024 * 1024 * 1024UL)
static uint64_t *ram;



static uint64_t shadow_register_w[2];
static uint64_t shadow_register_r[2];
static uint64_t shadow_addr_w;
static uint64_t shadow_addr_r;
static uint64_t shadow_en_w;
static uint64_t shadow_en_r;

void falling_edge_chk(struct sram* sram) {
	if ( en_w ) {
		assert( data_wstrb == 0xFFFF );
		shadow_register_w[0] = data_w[0];
		shadow_register_w[1] = data_w[1];
		shadow_addr_w = addr_w
	}
	shadow_en_w = en_w;

	if ( en_r ) {
		shadow_register_r[0] = ram[addr_r];
		shadow_register_r[1] = ram[addr_r] >> 64;
		shadow_addr_r = addr_r
	}
	shadow_en_r = en_r;
}

void raising_edge_opr(struct sram* sram) {
	if ( shadow_en_w ) {
		ram[addr_r] = shadow_register_w[1] << 64 | shadow_register_w[0];
	}
	if ( shadow_en_r ) {
		data_r = shadow_register_r[1] << 64 | shadow_register_r[0];
	}
}







void init_ram(const char *img) {
	assert(img != NULL);
	printf("The image is %s\n", img);

	printf("Using simulated %luMB RAM\n", EMU_RAM_SIZE / (1024 * 1024));
	ram = (uint64_t *)mmap(NULL, EMU_RAM_SIZE, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
	if ( ram == (uint64_t *)MAP_FAILED ) {
		printf("Cound not mmap 0x%lx bytes\n", EMU_RAM_SIZE);
		assert(0);
	}

	FILE *fp = fopen(img, "rb");
	if (fp == NULL) {
	  printf("Can not open '%s'\n", img);
	  assert(0);
	}

	fseek(fp, 0, SEEK_END);
	img_size = ftell(fp);
	if ( img_size > EMU_RAM_SIZE ) {
	  img_size = EMU_RAM_SIZE;
	}

	fseek(fp, 0, SEEK_SET);

	if ( 1 != fread(ram, img_size, 1, fp) ) {
		assert(0);
	}


	fclose(fp);

  pthread_mutex_init(&ram_mutex, 0);
}




void ram_finish() {
  munmap(ram, EMU_RAM_SIZE);

  pthread_mutex_destroy(&ram_mutex);
}




