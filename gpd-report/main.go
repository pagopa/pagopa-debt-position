package main

import (
	"encoding/json"
	"fmt"
	"go-sql/repository"
	"log"
	"os"
	"strconv"
	"time"

	"github.com/slack-go/slack"
)

var my_repository *repository.Repository
var days int
var slackURL string

func init() {
	var err error
	slackURL = os.Getenv("SLACK_URL")
	daysStr := os.Getenv("DAYS")
	if daysStr == "" {
		daysStr = "7" // Default value
		fmt.Println("DAYS environment variable is not set. Using default value:", daysStr)
	}
	days, err = strconv.Atoi(daysStr)
	if err != nil {
		log.Fatal("Error converting DAYS environment variable to integer:", err)
	}

	my_repository, err = repository.NewRepository()
	if err != nil {
		log.Fatal(err)
	}
}

func main() {
	// GetPositionLastDays <start>
	countCreated, err := my_repository.GetPositionsCounter(days)
	if err != nil {
		log.Fatal(err)
	}
	// GetPositionLastDays <end>

	// GetPaidOptionsCounter <start>
	countPaid, err := my_repository.GetPaidOptionsCounter(days)
	if err != nil {
		log.Fatal(err)
	}
	// GetPaidOptionsCounter <end>

	// GetPositionLastDays <start>
	countCreatedLastYear, err := my_repository.GetPositionsCounter(365)
	if err != nil {
		log.Fatal(err)
	}
	// GetPositionLastDays <end>

	// GetPaidOptionsCounter <start>
	countPaidLastYear, err := my_repository.GetPaidOptionsCounter(365)
	if err != nil {
		log.Fatal(err)
	}
	// GetPaidOptionsCounter <end>

	// TopCompany <start>
	var top_companies string
	companyData, err := my_repository.TopCompany(days)
	if err != nil {
		log.Fatal(err)
	}
	for _, data := range companyData {
		top_companies += fmt.Sprintf("%s, positions: %d\n", data.CompanyName, data.Count)
	}
	// TopCompany <end>

	// TopCompany <start>
	var top_companies_last_year string
	companyDataLastYear, err := my_repository.TopCompany(365)
	if err != nil {
		log.Fatal(err)
	}
	for _, data := range companyDataLastYear {
		top_companies_last_year += fmt.Sprintf("%s, positions: %d\n", data.CompanyName, data.Count)
	}
	// TopCompany <end>

	concatenated := fmt.Sprintf("\n• Created debt positions %d \n• Paid payment options %d \n *Top organizations*\n %s \n*Last 365 days*\n• Created debt positions %d \n• Paid payment options %d\n *Top organizations*\n %s",
		countCreated, countPaid, top_companies, countCreatedLastYear, countPaidLastYear, top_companies_last_year)
	var title string = fmt.Sprintf("GPD Report last %d days", days)
	attachment := slack.Attachment{
		Color:         "good",
		Fallback:      "You successfully posted by Incoming Webhook URL!",
		AuthorName:    title,
		AuthorSubname: "pagoPA",
		AuthorLink:    "<>",
		AuthorIcon:    "<>",
		Text:          concatenated,
		Footer:        "GPD-report",
		FooterIcon:    "https://platform.slack-edge.com/img/default_application_icon.png",
		Ts:            json.Number(strconv.FormatInt(time.Now().Unix(), 10)),
	}
	msg := slack.WebhookMessage{
		Attachments: []slack.Attachment{attachment},
	}

	err = slack.PostWebhook(slackURL, &msg)
	if err != nil {
		fmt.Println(err)
	}
}
