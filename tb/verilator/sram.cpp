/*
* @Author: Ruige Lee
* @Date:   2021-09-02 14:56:55
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-09-02 16:31:50
*/



void sram_init(  ) {

}


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


