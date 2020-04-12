#include <stdio.h> /* for stderr */
#include <stdlib.h> /* for exit() */
#include "types.h"
#include "utils.h"
#include "riscv.h"

void execute_rtype(Instruction, Processor *);
void execute_itype_except_load(Instruction, Processor *);
void execute_branch(Instruction, Processor *);
void execute_jal(Instruction, Processor *);
void execute_jalr(Instruction, Processor *);
void execute_load(Instruction, Processor *, Byte *);
void execute_store(Instruction, Processor *, Byte *);
void execute_ecall(Processor *, Byte *);
void execute_utype(Instruction, Processor *);

void execute_instruction(Instruction inst, Processor *p UNUSED, Byte *memory UNUSED) {
    switch(inst.opcode.opcode) { 
        case 0x33: execute_rtype(inst,p); break;
        case 0x03: execute_load(inst,p,memory); break;
        case 0x13: execute_itype_except_load(inst,p); break;
        case 0x67: execute_jalr(inst,p); break;
        case 0x73: execute_ecall(p,memory); break;
        case 0x23: execute_store(inst,p,memory); break;
        case 0x63: execute_branch(inst,p); break;
        case 0x6f: execute_jal(inst,p); break;
        case 0x17: execute_utype(inst,p); break;
        case 0x37: execute_utype(inst,p); break;
        default: 
            handle_invalid_instruction(inst);
            exit(-1);
            break;
    }
}

void execute_rtype(Instruction inst, Processor *p UNUSED) {
    int rd,rs1,rs2,tp,a,b;
    tp = (inst.rtype.funct3 << 8)+inst.rtype.funct7;
    rd = inst.rtype.rd, rs1=inst.rtype.rs1, rs2=inst.rtype.rs2;
    a=p->R[rs1],b=p->R[rs2];
    if(!rd) return;
    switch(tp) { 
        case 0x000: p->R[rd]=p->R[rs1]+p->R[rs2]; break;
        case 0x001: p->R[rd]=a*b; break;
        case 0x020: p->R[rd]=p->R[rs1]-p->R[rs2]; break;
        case 0x100: p->R[rd]=p->R[rs1]<<p->R[rs2]; break;
        case 0x101: p->R[rd]=((int64_t)a*(int64_t)b)>>32; break;
        case 0x200: p->R[rd]=(((int)p->R[rs1]) < (int)(p->R[rs2])); break;
        case 0x300: p->R[rd]=(p->R[rs1]<p->R[rs2]); break;
        case 0x400: p->R[rd]=p->R[rs1]^p->R[rs2]; break;
        case 0x401: p->R[rd]=(((int)p->R[rs1]) / ((int)p->R[rs2])); break;
        case 0x500: p->R[rd]=p->R[rs1]>>p->R[rs2]; break;
        case 0x520: p->R[rd]=((int)p->R[rs1])>>((int)p->R[rs2]); break;
        case 0x600: p->R[rd]=p->R[rs1]|p->R[rs2]; break;
        case 0x601: p->R[rd]=(((int)p->R[rs1]) % ((int)p->R[rs2])); break;
        case 0x700: p->R[rd]=p->R[rs1]&p->R[rs2]; break;
        default:
            handle_invalid_instruction(inst);
            exit(-1);
            break;
    }
    p->PC += 4;
}

void execute_itype_except_load(Instruction inst, Processor *p UNUSED) {
    UNUSED int rd,rs1,imm;
    imm = get_imm_operand(inst);
    rd = inst.itype.rd;
    rs1 = inst.itype.rs1;
    switch(inst.itype.funct3) {
        case 0: p->R[rd]=p->R[rs1]+imm; break;
        case 1: p->R[rd]=p->R[rs1]<<imm; break;
        case 2: p->R[rd]=(((int)p->R[rs1])<imm); break;
        case 3: p->R[rd]=(p->R[rs1]<(unsigned)imm); break;
        case 4: p->R[rd]=p->R[rs1]^imm; break;
        case 5: {
            if(imm > 32) p->R[rd]=((int)p->R[rs1])>>(imm-1024);
            else p->R[rd]=p->R[rs1]>>(unsigned)imm;
            break;
        }
        case 6: p->R[rd]=p->R[rs1]|imm; break;
        case 7: p->R[rd]=p->R[rs1]&imm; break;
        default:
            handle_invalid_instruction(inst);
            exit(-1);
            break;
    }
    p->PC += 4;
}

void execute_ecall(Processor *p UNUSED, Byte *memory UNUSED) {
    switch(p->R[10]) {
        case 1: printf("%d",(int)p->R[11]); break;
        case 4: {
            int ptr = p->R[11];
            while(*(memory+ptr)){
                printf("%c",*(memory+ptr));
                ++ptr;
                if(ptr >= MEMORY_SPACE) break;
            }
            break;
        }
        case 10: printf("exiting the simulator\n"); exit(0); break;
        case 11: printf("%c",(char)p->R[11]); break;
        default: /* undefined ecall */
            printf("Illegal ecall number %d\n", -1); /* What stores the ecall arg? */
            exit(-1);
            break;
    }
    p->PC += 4;
}

void execute_branch(Instruction inst, Processor *p UNUSED) {
    UNUSED int off,rs1,rs2;
    rs1 = inst.sbtype.rs1;
    rs2 = inst.sbtype.rs2;
    off = get_branch_offset(inst)<<1;
    switch(inst.sbtype.funct3) { 
        case 0: p->PC=(p->R[rs1]==p->R[rs2]?p->PC+off:p->PC+4); break;
        case 1: p->PC=(p->R[rs1]!=p->R[rs2]?p->PC+off:p->PC+4); break;
        case 4: p->PC=(((int)p->R[rs1])<((int)p->R[rs2])?p->PC+off:p->PC+4); break;
        case 5: p->PC=(((int)p->R[rs1])>=((int)p->R[rs2])?p->PC+off:p->PC+4); break;
        case 6: p->PC=(p->R[rs1]<p->R[rs2]?p->PC+off:p->PC+4); break;
        case 7: p->PC=(p->R[rs1]>=p->R[rs2]?p->PC+off:p->PC+4); break;
        default:
            handle_invalid_instruction(inst);
            exit(-1);
            break;
    }
}

void execute_load(Instruction inst, Processor *p UNUSED, Byte *memory UNUSED) {
    int rd,rs1,imm;
    rd = inst.itype.rd;
    rs1 = inst.itype.rs1;
    imm = get_imm_operand(inst);
    switch(inst.itype.funct3) {
        case 0:{
            int temp = load(memory,p->R[rs1]+imm,1,0);
            if(temp & (1<<7)) temp |= (-256);
            p->R[rd] = temp;
            break;
        }
        case 1:{
            int temp = load(memory,p->R[rs1]+imm,2,0);
            if(temp & (1<<15)) temp |= (-65536);
            p->R[rd] = temp;
            break;
        }
        case 2: p->R[rd] = load(memory,p->R[rs1]+imm,4,0); break;
        case 4: p->R[rd] = load(memory,p->R[rs1]+imm,1,0); break;
        case 5: p->R[rd] = load(memory,p->R[rs1]+imm,2,0); break;
        default:
            handle_invalid_instruction(inst);
            exit(-1);
            break;
    }
    p->PC += 4;
}

void execute_store(Instruction inst, Processor *p UNUSED, Byte *memory UNUSED) {
    unsigned prs1,prs2;
    int off;
    prs1 = p->R[inst.stype.rs1];
    prs2 = p->R[inst.stype.rs2];
    off = get_store_offset(inst);
    switch(inst.stype.funct3) {
        case 0: store(memory,prs1+off,1,prs2,0); break;
        case 1: store(memory,prs1+off,2,prs2,0); break;
        case 2: store(memory,prs1+off,4,prs2,0); break;
        default:
            handle_invalid_instruction(inst);
            exit(-1);
            break;
    }
    p->PC += 4;
}

void execute_jal(Instruction inst UNUSED, Processor *p UNUSED) {
    int ham = get_jump_offset(inst)<<1;
    if(ham%4){
        handle_invalid_instruction(inst);
        exit(-1);
    }
    p->R[inst.ujtype.rd] = p->PC+4;
    p->PC = p->PC+(get_jump_offset(inst)<<1);
}

void execute_jalr(Instruction inst UNUSED, Processor *p UNUSED) {
    int imm = get_imm_operand(inst);
    int prs1 = p->R[inst.itype.rs1];
    if(imm%4){
        handle_invalid_instruction(inst);
        exit(-1);
    }
    p->R[inst.itype.rd] = p->PC+4;
    p->PC = prs1+get_imm_operand(inst);
}

void execute_utype(Instruction inst, Processor *p UNUSED) {
    unsigned imm,rd;
    imm = inst.utype.imm;
    rd = inst.utype.rd;
    switch(inst.utype.opcode) { 
        case 0x17: p->R[rd] = p->PC+(imm<<12); break;
        case 0x37: p->R[rd] = imm<<12; break;
        default:
            handle_invalid_instruction(inst);
            exit(-1);
            break;
    }
    p->PC += 4;
}

/* Checks that the address is aligned correctly */
int check(Address address, Alignment alignment) {
    if(address > 0 && address < MEMORY_SPACE) {
        if(alignment == LENGTH_BYTE) return 1;
        else if(alignment == LENGTH_HALF_WORD) 
            return address%2 == 0;
        else if(alignment == LENGTH_WORD)
            return address%4 ==0;
    }
    return 0;
}

void store(Byte *memory UNUSED, Address address, Alignment alignment, Word value UNUSED, int check_align) {
    if((check_align && !check(address, alignment)) || (address >= MEMORY_SPACE))
        handle_invalid_write(address);

    *(memory+address) = (unsigned)value & (255);
    if(alignment>=2 && (address <MEMORY_SPACE-1)) *(memory+address+1) = (value & (65280))>>8;
    if(alignment>2 && (address <MEMORY_SPACE-3)) *(memory+address+2) = (value & (16711680))>>16;
    if(alignment>2 && (address <MEMORY_SPACE-3)) *(memory+address+3) = (value & (-16777216))>>24;
}

Word load(Byte *memory UNUSED, Address address, Alignment alignment, int check_align) {
    Word data = 0; /* initialize our return value to zero */

    if((check_align && !check(address, alignment)) || (address >= MEMORY_SPACE))
        handle_invalid_read(address);
    
    data += (unsigned)*(memory+address);
    if(alignment>=LENGTH_HALF_WORD && (address <MEMORY_SPACE-1)) data += ((unsigned)*(memory+address+1))<<8;
    if(alignment==LENGTH_WORD && (address <MEMORY_SPACE-3)) data += (((unsigned)*(memory+address+2))<<16)+(((unsigned)*(memory+address+3))<<24);
    return data;
}
