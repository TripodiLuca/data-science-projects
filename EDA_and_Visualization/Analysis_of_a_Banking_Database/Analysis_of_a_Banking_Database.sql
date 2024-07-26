/******************************************
*      Analysis of a Banking Database     *
******************************************/

-- Description --
/*
In this project a denormalized table was generated containing behavioral indicators of clients, calculated based on their banking transactions and the banking products owned.
The aim is to create features for a potential supervised machine learning model.
*/

-- Indicators --
/*
The following indicators related to each customer ID have been created:
1 - Age
2A - Number of outgoing transactions on all accounts
2B - Number of incoming transactions on all accounts
3A - Amount transacted out on all accounts
3B - Amount transacted in on all accounts
4 - Total number of accounts owned
5 - Number of accounts owned by type (an indicator for each type)
6A - Number of outgoing transactions by type (an indicator for each type)
6B - Number of incoming transactions by type (an indicator for each type)
7A - Amount transacted out by account type (an indicator for each type)
7B - Amount transacted in by account type (an indicator for each type)
*/


/* MySQL version used */
select version(); -- 8.0.37


/*--------------------------------------------------------------------------------------------------------------------------------------------------*/

/*----------------*/
/* 1 - Client Age */
/*----------------*/

create temporary table banca.t1_client_age (

	-- Display client ID, birth date and current age
	select
		id_cliente,
		data_nascita,
		timestampdiff(year, data_nascita, current_date()) as client_age -- timestamp difference in days converted in years
	from banca.cliente
	
);



/*----------------------------------------------------------------------*/
/* 2A - Number of Outgoing Transactions on All Accounts for Each Client */

create temporary table banca.t2a_number_outgoing_transactions (

	-- Display client ID and count of sign occurrences
	select
		clients.id_cliente,
		count(transaction_types.segno) as number_outgoing_transactions
	from banca.cliente clients

	-- Join accounts table based on same client ID
	inner join banca.conto accounts
	on clients.id_cliente = accounts.id_cliente

	-- Join transactions table based on same account ID
	inner join banca.transazioni transactions
	on accounts.id_conto = transactions.id_conto

	-- Join transaction types table based on same transaction type ID
	inner join banca.tipo_transazione transaction_types
	on transactions.id_tipo_trans = transaction_types.id_tipo_transazione

	-- Select only transactions with negative sign
	where segno = '-'

	-- Group by client ID
	group by clients.id_cliente
	
);
	


/*---------------------------------------------------------------------*/
/* 2B - Number of Ingoing Transactions on All Accounts for Each Client */
/*---------------------------------------------------------------------*/

create temporary table banca.t2b_number_ingoing_transactions (

	-- Display client ID and count of sign occurrences
	select	
		clients.id_cliente,
		count(transaction_types.segno) as number_ingoing_transactions
	from banca.cliente clients

	-- Join accounts table based on same client ID
	inner join banca.conto accounts
	on clients.id_cliente = accounts.id_cliente

	-- Join transactions table based on same account ID
	inner join banca.transazioni transactions
	on accounts.id_conto = transactions.id_conto

	-- Join transaction types table based on same transaction type ID
	inner join banca.tipo_transazione transaction_types
	on transactions.id_tipo_trans = transaction_types.id_tipo_transazione

	-- Select only transactions with positive sign
	where segno = '+'

	-- Group by client ID
	group by clients.id_cliente
	
);



/*---------------------------------------------------------------------*/
/* 3A - Amount Transacted Outgoing Across All Accounts for Each Client */
/*---------------------------------------------------------------------*/

create temporary table banca.t3a_total_outgoing_amount (

	-- Display client ID and total outgoing amount
	select
		clients.id_cliente,
		sum(importo) as total_outgoing_amount
	from banca.cliente clients

	-- Join accounts table based on same client ID
	inner join banca.conto accounts
	on clients.id_cliente = accounts.id_cliente

	-- Join transactions table based on same account ID
	inner join banca.transazioni transactions
	on accounts.id_conto = transactions.id_conto

	-- Join transaction types table based on same transaction type ID
	inner join banca.tipo_transazione transaction_types
	on transactions.id_tipo_trans = transaction_types.id_tipo_transazione

	-- Select only transactions with negative amount
	where importo < 0 -- (same as segno = '-')

	-- Group by client ID
	group by clients.id_cliente
	
);



/*--------------------------------------------------------------------*/
/* 3B - Amount Transacted Ingoing Across All Accounts for Each Client */
/*--------------------------------------------------------------------*/

create temporary table banca.t3b_total_ingoing_amount (

	-- Display client ID and total ingoing amount
	select
		clients.id_cliente,
		sum(importo) as total_ingoing_amount
	from banca.cliente clients

	-- Join accounts table based on same client ID
	inner join banca.conto accounts
	on clients.id_cliente = accounts.id_cliente

	-- Join transactions table based on same account ID
	inner join banca.transazioni transactions
	on accounts.id_conto = transactions.id_conto

	-- Join transaction types table based on same transaction type ID
	inner join banca.tipo_transazione transaction_types
	on transactions.id_tipo_trans = transaction_types.id_tipo_transazione

	-- Select only transactions with positive amount
	where importo > 0 -- (same as segno = '+')

	-- Group by client ID
	group by clients.id_cliente
	
);



/*----------------------------------------------*/
/* 4 - Number of Owned Accounts for Each Client */
/*----------------------------------------------*/

create temporary table banca.t4_number_accounts (

	-- Display client ID and number of accounts
	select
		clients.id_cliente,
		count(accounts.id_conto) as number_accounts
	from banca.cliente clients

	-- Join accounts table based on same client ID
	inner join banca.conto accounts
	on clients.id_cliente = accounts.id_cliente

	-- Group by client ID
	group by clients.id_cliente
	
);
 
 
 
/*----------------------------------------------------------------*/
/* 5 - Number of Owned Accounts for Each Client for Account Types */
/*----------------------------------------------------------------*/

create temporary table banca.t5_number_accounts_by_type (

	-- Display client ID and number of accounts by type
	select
		clients.id_cliente,
		sum(case when id_tipo_conto = 0 then 1 else 0 end) as number_Base,
		sum(case when id_tipo_conto = 1 then 1 else 0 end) as number_Business,
		sum(case when id_tipo_conto = 2 then 1 else 0 end) as number_Privati,
		sum(case when id_tipo_conto = 3 then 1 else 0 end) as number_Famiglie
	from banca.cliente clients

	-- Join accounts table based on same client ID
	inner join banca.conto accounts
	on clients.id_cliente = accounts.id_cliente

	-- Group by client ID
	group by clients.id_cliente
	
);



/*--------------------------------------------------------------*/
/* 6A - Number of Outgoing Transactions by Type for Each Client */
/*--------------------------------------------------------------*/

create temporary table banca.t6a_number_of_outgoing_transactions_by_type (

	-- Display client ID and number of outgoing transactions by type
	select	
		clients.id_cliente,
		sum(case when id_tipo_trans = 3 then 1 else 0 end) as number_Amazon,
		sum(case when id_tipo_trans = 4 then 1 else 0 end) as number_Mutuo,
		sum(case when id_tipo_trans = 5 then 1 else 0 end) as number_Hotel,
		sum(case when id_tipo_trans = 6 then 1 else 0 end) as number_Aereo,
		sum(case when id_tipo_trans = 7 then 1 else 0 end) as number_Supermercato
	from banca.cliente clients

	-- Join accounts table based on same client ID
	inner join banca.conto accounts
	on clients.id_cliente = accounts.id_cliente

	-- Join transactions table based on same account ID
	inner join banca.transazioni transactions
	on accounts.id_conto = transactions.id_conto

	-- Join transaction types table based on same transaction type ID
	inner join banca.tipo_transazione transaction_types
	on transactions.id_tipo_trans = transaction_types.id_tipo_transazione

	-- Select only transactions with negative sign
	where segno = '-'

	-- Group by client ID
	group by clients.id_cliente
	
);



/*-------------------------------------------------------------*/
/* 6B - Number of Ingoing Transactions by Type for Each Client */
/*-------------------------------------------------------------*/

create temporary table banca.t6b_number_of_ingoing_transactions_by_type (

	-- Display client ID and number of ingoing transactions by type
	select
		clients.id_cliente,
		sum(case when id_tipo_trans = 0 then 1 else 0 end) as number_Stipendio,
		sum(case when id_tipo_trans = 1 then 1 else 0 end) as number_Pensione,
		sum(case when id_tipo_trans = 2 then 1 else 0 end) as number_Dividendi
	from banca.cliente clients

	-- Join accounts table based on same client ID
	inner join banca.conto accounts
	on clients.id_cliente = accounts.id_cliente

	-- Join transactions table based on same account ID
	inner join banca.transazioni transactions
	on accounts.id_conto = transactions.id_conto

	-- Join transaction types table based on same transaction type ID
	inner join banca.tipo_transazione transaction_types
	on transactions.id_tipo_trans = transaction_types.id_tipo_transazione

	-- Select only transactions with positive sign
	where segno = '+'

	-- Group by client ID
	group by clients.id_cliente
	
);



/*-----------------------------------------------------------------*/
/* 7A - Amount Transacted Outgoing by Account Type for Each Client */
/*-----------------------------------------------------------------*/

create temporary table banca.t7a_amount_outgoing_by_account_type (

	-- Display client ID and outgoing amount by account type
	select
		clients.id_cliente,
		sum(case when id_tipo_conto = 0 then importo else 0 end) as amount_outgoing_Base,
		sum(case when id_tipo_conto = 1 then importo else 0 end) as amount_outgoing_Business,
		sum(case when id_tipo_conto = 2 then importo else 0 end) as amount_outgoing_Privati,
		sum(case when id_tipo_conto = 3 then importo else 0 end) as amount_outgoing_Famiglie
	from banca.cliente clients

	-- Join accounts table based on same client ID
	inner join banca.conto accounts
	on clients.id_cliente = accounts.id_cliente

	-- Join transactions table based on same account ID
	inner join banca.transazioni transactions
	on accounts.id_conto = transactions.id_conto

	-- Join transaction types table based on same transaction type ID
	inner join banca.tipo_transazione transaction_types
	on transactions.id_tipo_trans = transaction_types.id_tipo_transazione

	-- Select only transactions with negative sign
	where segno = '-'

	-- Group by client ID
	group by clients.id_cliente
	
);



/*----------------------------------------------------------------*/
/* 7B - Amount Transacted Ingoing by Account Type for Each Client */
/*----------------------------------------------------------------*/

create temporary table banca.t7b_amount_ingoing_by_account_type (

	-- Display client ID and ingoing amount by account type
	select
		clients.id_cliente,
		sum(case when id_tipo_conto = 0 then importo else 0 end) as amount_ingoing_Base,
		sum(case when id_tipo_conto = 1 then importo else 0 end) as amount_ingoing_Business,
		sum(case when id_tipo_conto = 2 then importo else 0 end) as amount_ingoing_Privati,
		sum(case when id_tipo_conto = 3 then importo else 0 end) as amount_ingoing_Famiglie
	from banca.cliente clients

	-- Join accounts table based on same client ID
	inner join banca.conto accounts
	on clients.id_cliente = accounts.id_cliente

	-- Join transactions table based on same account ID
	inner join banca.transazioni transactions
	on accounts.id_conto = transactions.id_conto

	-- Join transaction types table based on same transaction type ID
	inner join banca.tipo_transazione transaction_types
	on transactions.id_tipo_trans = transaction_types.id_tipo_transazione

	-- Select only transactions with positive sign
	where segno = '+'

	-- Group by client ID
	group by clients.id_cliente
	
);



/*-------------*/
/* Final Query */
/*-------------*/

select
    t1.id_cliente,
    t1.client_age,
    t2a.number_outgoing_transactions,
    t2b.number_ingoing_transactions,
    t3a.total_outgoing_amount,
    t3b.total_ingoing_amount,
    t4.number_accounts,
    t5.number_Base,
    t5.number_Business,
    t5.number_Privati,
    t5.number_Famiglie,
    t6a.number_Amazon,
    t6a.number_Mutuo,
    t6a.number_Hotel,
    t6a.number_Aereo,
    t6a.number_Supermercato,
    t6b.number_Stipendio,
    t6b.number_Pensione,
    t6b.number_Dividendi,
    t7a.amount_outgoing_Base,
    t7a.amount_outgoing_Business,
    t7a.amount_outgoing_Privati,
    t7a.amount_outgoing_Famiglie,
    t7b.amount_ingoing_Base,
    t7b.amount_ingoing_Business,
    t7b.amount_ingoing_Privati,
    t7b.amount_ingoing_Famiglie
from banca.t1_client_age t1
left join banca.t2a_number_outgoing_transactions            t2a on t1.id_cliente = t2a.id_cliente
left join banca.t2b_number_ingoing_transactions             t2b on t1.id_cliente = t2b.id_cliente
left join banca.t3a_total_outgoing_amount                   t3a on t1.id_cliente = t3a.id_cliente
left join banca.t3b_total_ingoing_amount                    t3b on t1.id_cliente = t3b.id_cliente
left join banca.t4_number_accounts                          t4  on t1.id_cliente = t4.id_cliente
left join banca.t5_number_accounts_by_type                  t5  on t1.id_cliente = t5.id_cliente
left join banca.t6a_number_of_outgoing_transactions_by_type t6a on t1.id_cliente = t6a.id_cliente
left join banca.t6b_number_of_ingoing_transactions_by_type  t6b on t1.id_cliente = t6b.id_cliente
left join banca.t7a_amount_outgoing_by_account_type         t7a on t1.id_cliente = t7a.id_cliente
left join banca.t7b_amount_ingoing_by_account_type          t7b on t1.id_cliente = t7b.id_cliente;


/* Delete all temporary tables */
drop temporary table if exists banca.t1_client_age;
drop temporary table if exists banca.t2a_number_outgoing_transactions;
drop temporary table if exists banca.t2b_number_ingoing_transactions;
drop temporary table if exists banca.t3a_total_outgoing_amount;
drop temporary table if exists banca.t3b_total_ingoing_amount;
drop temporary table if exists banca.t4_number_accounts;
drop temporary table if exists banca.t5_number_accounts_by_type;
drop temporary table if exists banca.t6a_number_of_outgoing_transactions_by_type;
drop temporary table if exists banca.t6b_number_of_ingoing_transactions_by_type;
drop temporary table if exists banca.t7a_amount_outgoing_by_account_type;
drop temporary table if exists banca.t7b_amount_ingoing_by_account_type;