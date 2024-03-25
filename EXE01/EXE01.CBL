       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EXE01.
       DATE-WRITTEN.  20/03/2024.
       DATE-COMPILED.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SOURCE-COMPUTER. IBM-PC.
           OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADCLI1   ASSIGN TO DISK
           ORGANIZATION IS  LINE  SEQUENTIAL.
           
           SELECT CADCLI2   ASSIGN TO DISK
           ORGANIZATION IS  LINE  SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD CADCLI1
           LABEL  RECORD  ARE STANDARD
           VALUE  OF FILE-ID IS "CADCLI1.DAT".
       
       01 REG-ENT.

           03 COD-ENT     PIC 9(05).
           03 NOME-ENT    PIC X(20).
       
       FD CADCLI2
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADCLI2.DAT".

       01 REG-SAI.
           03 NOME-SAI     PIC X(20).
           03 COD-SAI      PIC 9(05).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ     PIC X(03) VALUE "NAO".
       
       PROCEDURE DIVISION.
       PGM-EM01EX01.
           PERFORM INICIO.

           PERFORM PRINCIPAL
                  UNTIL FIM-ARQ EQUAL "SIM".

           PERFORM TERMINO.
           STOP RUN.

       INICIO.
           OPEN INPUT CADCLI1
                OUTPUT CADCLI2.
           PERFORM LEITURA.

       LEITURA.
           READ CADCLI1
                AT END
                MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM GRAVACAO.
           PERFORM LEITURA.

       GRAVACAO.
           MOVE COD-ENT TO COD-SAI.
           MOVE NOME-ENT TO NOME-SAI.
           WRITE REG-SAI.

       TERMINO.
           CLOSE  CADCLI1
                  CADCLI2.
