       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EXE03.
       DATE-WRITTEN.  20/03/2024.
       DATE-COMPILED.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SOURCE-COMPUTER. IBM-PC.
           OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADALU   ASSIGN TO DISK
           ORGANIZATION IS  LINE  SEQUENTIAL.
           
           SELECT CADATU   ASSIGN TO DISK
           ORGANIZATION IS  LINE  SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD CADALU
           LABEL  RECORD  ARE STANDARD
           VALUE  OF FILE-ID IS "CADALU.DAT".
       
       01 REG-ENT.
           03 NUMERO-ENT       PIC 9(05).
           03 NOME-ENT         PIC X(20).
           03 NOTA1-ENT        PIC 9(02)V99.
           03 NOTA2-ENT        PIC 9(02)V99.
           03 NOTA3-ENT        PIC 9(02)V99.

       
       FD CADATU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADATU.DAT".

       01 REG-SAI.
           03 NUMERO-SAI       PIC 9(05).
           03 MEDIA-SAI        PIC 9(02)V99.

       WORKING-STORAGE SECTION.
       77 FIM-ARQ     PIC X(03) VALUE "NAO".
       77 SOMA-NOTA   PIC 9(02)V99 VALUE ZEROS.
       77 WMEDIA      PIC 99V99   VALUE ZEROS.
       
       PROCEDURE DIVISION.
       PGM-EM01EX03.
           PERFORM INICIO.
           PERFORM PRINCIPAL
                  UNTIL FIM-ARQ EQUAL "SIM".
           PERFORM TERMINO.
           STOP RUN.

       INICIO.
           OPEN INPUT CADALU
                OUTPUT CADATU.
           PERFORM LEITURA.

       LEITURA.
           READ CADALU
                AT END
                MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM CALCMEDIA.
           PERFORM GRAVACAO.
           PERFORM LEITURA.
       
       CALCMEDIA.
           ADD  NOTA1-ENT
                NOTA2-ENT
                NOTA3-ENT
                  GIVING SOMA-NOTA.
           DIVIDE SOMA-NOTA BY 3 GIVING WMEDIA.

       GRAVACAO.
           MOVE NUMERO-ENT TO NUMERO-SAI.
           MOVE WMEDIA TO MEDIA-SAI.
           WRITE REG-SAI.

       TERMINO.
           CLOSE  CADALU
                  CADATU.
