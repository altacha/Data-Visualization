# Data-Visualization
#Saya akan menganalisa "Jumlah Customer terbanyak dari suatu State". Ekspektasi dari kesimpulan nanti, State dengan customer yang paling banyak, perlu diperhatikan lebih servicenya. Salah satunya diperbanyak kurir dan ditingkatkan kualitas pengiriman di state tersebut.  # Prosedur Pengolahan data yang saya pakai hingga menjadi visualisasi adalah berikut 1. Check Missing Value 2. Data Wrangling (Tame and Tidying) 3. Data Transformasi 4. Visualisasi 5. Kesimpulan
#Load package yang dibutuhkan
```{r}
library(readr)
library(dplyr)
library(ggplot2)
library(assertive)
```
#Membaca masing-masing file
```{r}
list.customer <- read.csv(file='olist_customers_dataset.csv')
list.order.item <- read.csv(file='olist_order_items_dataset.csv')
list.order.pay <- read.csv(file='olist_order_payments_dataset.csv')
list.order.delivery <- read.csv(file='olist_orders_dataset.csv')
list.product <- read.csv(file='olist_products_dataset.csv')
list.product.translate <- read.csv(file='product_category_name_translation.csv')
```
#Kemudian Menampilkan masing-masing file
```{r}
list.customer
list.order.item
list.order.pay
list.order.delivery
list.product
list.product.translate
```
#Setelah membaca semua file, lalu dipilih file-file yang berkaitan (relate) dengan data yang akan saya analisa. Dalam analisa kali ini, saya akan mengambil 3 data; 
1. data "list.customer" untuk membaca customer.id dan customer_state
2. data "list.order.item" untuk membaca order_id dan order_item_id 
3. data "list.order.delivery" untuk membaca customer.id dan order_id
Dengan data-data ini saya dapat menganalisa hubungan State dimana customer tinggal (melalui customer_state) dengan jumlah pembelian customer (order_item_id)

#DATA WRANGLING

#CHECK MISSING VALUE 

#Untuk Mengecek Missing Value, menggunakan fungsi "is.na" dan dijumlahkan untuk memudahkan pengecekan. 
```{r}
sum(is.na(list.customer))
sum(is.na(list.order.item))
sum(is.na(list.order.delivery))
```
#Hasil pengecekan missing value ketiga diatas adalah "0" yang artinya tidak ada data yang hilang / tidak terbaca. Sehingga dapat langsung dilanjutkan ke Pengecekan berikutnya

#DATA CLEANING 

#Sebelum mentransformasikan data, pada analisa kali ini, saya harus mengecek keseragaman dan duplikasi semua data set.
#Langkah pertama data wrangling yaitu pengecekan tipe data numerik para dataset "order_item_id"
```{r}
assert_is_numeric(list.order.item$order_item_id)
```
#Tidak ada result / Output dari pengecekan ini, artinya data sudah seragam

#Langkah berikutnya pengecekan duplikasi data pada dataset "order_item_id" dan menjumlahkan data yang terduplikasi tersebut untuk memudahkan pengecekan
```{r}
sum(duplicated(list.customer))
sum(duplicated(list.order.item))
sum(duplicated(list.order.delivery))
sum(duplicated(list.product))
```
#Hasil dari pengecekan duplikasi data adalah "0". Yang artinya tidak ada data yang terduplikasi. Sehingga dapat langsung dilanjukan ke data transformasi

#DATA TRANSFORMASI

#Menggabungkan (join) dataset-dataset yang saya butuhkan tadi, menjadi satu data yang berelasi antar lainnya. (saya simpan dalam field "joined data") 
```{r}
joined.data <- list.customer %>%
  inner_join(list.order.delivery, by= 'customer_id') %>% 
  inner_join(list.order.item,by='order_id') %>% 
  inner_join(list.product, by='product_id')
joined.data
```
#Setelah menggabungkan dataset-dataset diatas, saya memilih dataset-dataset yang saya butuhkan untuk menganalisa. Saya memilih (select) customer_state dan order_item_id. 
```{r}
selected.data <- joined.data %>%
  select(customer_state, order_item_id)

selected.data
```
#Dari hasil data yang sudah saya pilih (select), saya dapat menginformasikan bahwa berikut adalah data State dari setiap customer (tabel kiri) YANG telah membeli barang dengan kuantitas tertentu (tabel kanan). Dari data ini saya akan mengolah lagi untuk mengetahui TOTAL barang yang dibeli (count) oleh pelanggan berdasarkan State customer dari tersebut. 

```{r}
analysis.1 <- selected.data %>% 
  count(customer_state, wt=order_item_id, sort=TRUE) %>% 
  arrange(desc(n))
analysis.1 
```
#Kesimpulan dari tabel data diatas adalah State "SP" memiliki customer dengan pembelian barang paling banyak. Diikuti dengan State "RJ" dan "MG" dan seterusnya. 
#Kesimpulan berikutnya yaitu adanya gap (perbedaan) yang cukup besar pada jumlah pembelian customer state "SP" dengan customer state lain

#Untuk mengecilkan data berikut agar dapat divisualisasikan dengan jelas (dikarenakan jumlah state yang terlalu banyak, sehingga membuat visualisasi menjadi tidak rapi dan plot tidak dapat menampung semua data), saya akan mengambil 10 state teratas

```{r}
top_state <- selected.data %>% 
  filter(customer_state %in% c("SP", "RJ", "MG","RS","PR","SC","BA","GO","DF","ES")) %>% 
  count(customer_state, wt=order_item_id, sort=TRUE) %>% 
  arrange(desc(n))
top_state
```

#Berikut adalah hasil dari State dengan customer dengan pembelian terbanyak.

#Setelah itu saya ambil kembali jumlah customer tanpa memperhitungkan jumlah barang.
```{r}
top_state_uncounted <- selected.data %>% 
  filter(customer_state %in% c("SP", "RJ", "MG","RS","PR","SC","BA","GO","DF","ES"))
top_state_uncounted
```
#Dengan data ini saya mengolah untuk menjadi data tabel untuk memvisualisasikan data dengan barplot()

****Yang akan saya visualisasikan adalah Jumlah customer pada state, BUKAN jumlah item yang dibeli. ****


```{r}
tabled <- table(top_state_uncounted$customer_state)
tabled

```

```{r}
barplot(tabled,
        beside = TRUE,
        border = 'white',
        main = "Customer's Buying by State",
        xlab = "State Name",
        ylab = "Total Customer",
        space = 0.5,
        col = rainbow(10),
        ylim = c(0,60000),
        legend.text = rownames(tabled),
           args.legend = list(bty = "n",
             title = "State Name",
           x = "topleft",
           inset = c(0.05,0))
)
```

# Berikut adalah Grafik "Bar Plot" Pembelian Product berdasarkan Customer's State.
# Kesimpulan pada Visualisasi dari analisa berikut:
1. Kode State "SP" memiliki pembeli / costumer paling banyak, diikuti dengan Kode State "RJ" dan "MG"
2. Ada jgap yang sangat besar antara pembeli Kode State "SP" dengan Kode State lainnya. Dengan visualisasi ini dapat dilihat dengan jelas perbedaannya.

#Dengan melihat grafik ini saya bisa berekspektasi bahwa untuk mengimbangi demand yang tinggi, maka tingkat kualitas kurir, sumber daya, dan mobilitas transpotasi dari State "SP" akan meningkat DRASTIS di waktu yang akan datang, dibandingkan dengan State lainnya.

