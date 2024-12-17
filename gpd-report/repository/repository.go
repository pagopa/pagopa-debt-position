package repository

import (
	"database/sql"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"

	"github.com/lib/pq"
)

type Repository struct {
	db *sql.DB
}

func NewRepository() (*Repository, error) {
	url := os.Getenv("PG_URL")
	user := os.Getenv("PG_USER")
	pass := os.Getenv("PG_PASSWORD")
	split := strings.Split(url, "/")
	host := strings.Split(split[2], ":")[0]
	port := strings.Split(split[2], ":")[1]
	dbName := strings.Split(split[3], "?")[0]

	connStr := fmt.Sprintf("host=%s port=%s dbname=%s user=%s password=%s sslmode=require", host, port, dbName, user, pass)

	pq.ParseURL(connStr)
	db, err := sql.Open("postgres", connStr)
	if err != nil {
		return nil, err
	}

	pingErr := db.Ping()
	if pingErr != nil {
		log.Fatal(pingErr)
	}
	fmt.Println("Connected!")
	//defer db.Close()

	return &Repository{db}, nil
}

func (r *Repository) Close() error {
	return r.db.Close()
}

func (r *Repository) GetPositionsCounter(days int) (int, error) {
	var countCreated int

	query := "SELECT count(*) FROM apd.payment_position pp WHERE pp.inserted_date > NOW() - INTERVAL '" + strconv.Itoa(days) + " DAY'"

	// Execute the query
	err := r.db.QueryRow(query).Scan(&countCreated)
	if err != nil {
		return 0, err
	}

	return countCreated, nil
}

func (r *Repository) GetPaidOptionsCounter(days int) (int, error) {
	var countPaid int

	query := "SELECT count(*) FROM apd.payment_option po WHERE po.status = 'PO_PAID' AND po.payment_date > NOW() - INTERVAL '" + strconv.Itoa(days) + "DAY'"

	// Execute the query
	err := r.db.QueryRow(query).Scan(&countPaid)
	if err != nil {
		return 0, err
	}

	return countPaid, nil
}

func (r *Repository) TopCompany(days int) ([]CompanyData, error) {
	rows, err := r.db.Query("SELECT company_name, COUNT(*) FROM apd.payment_position pp WHERE pp.inserted_date > NOW() - INTERVAL '" + strconv.Itoa(days) + "' DAY GROUP BY company_name ORDER BY 2 DESC LIMIT 5")
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var companies []CompanyData

	for rows.Next() {
		var data CompanyData
		if err := rows.Scan(&data.CompanyName, &data.Count); err != nil {
			return nil, err
		}
		companies = append(companies, data)
	}

	return companies, nil
}

type CompanyData struct {
	CompanyName string
	Count       int
}

// example
func (r *Repository) QueryExample() error {
	rows, err := r.db.Query("SELECT * FROM table")
	if err != nil {
		return err
	}
	defer rows.Close()

	// Iterate over query results
	for rows.Next() {
		var column1 int
		var column2 string
		if err := rows.Scan(&column1, &column2); err != nil {
			return err
		}
		fmt.Printf("column1: %d, column2: %s\n", column1, column2)
	}
	return nil
}
