<script>
  import { getIdentifier } from './client.js'
  import { postData } from './postdata.js'
  import OrderLine from './OrderLine.svelte'
  import Address   from './Address.svelte'
  import Customer  from './Customer.svelte'
  import Button    from './Button.svelte'

  let shippingAddressSameAsAddress = true;
  let customer = {};
  let billingAddress = {};
  let shippingAddress = {};
  let orderLines = [];
  let order = {};

  async function submitOrder() {
    let command = {
      "tag": "PlaceOrderCmdDTO",
      "value": order
    };
    let data = await postData('http://localhost:3000/order-taking', command);
    console.log('submitOrder response: ' + JSON.stringify(data));
  }

  // function submitOrder() {
  //   console.log('Submit order');
  // }

  async function updateOrder() {
    const identifier = await getIdentifier();
    order = {
      identifier: identifier,
      customer: customer,
      billingAddress: billingAddress,
      shippingAddress: shippingAddress,
      orderLines: orderLines,
    };
    console.log(JSON.stringify(order));
  }

  let productCode = '';
  let quantity = '';
  async function addOrderLine() {
    const identifier = await getIdentifier();
    const orderLine = {
      identifier: identifier,
      productCode: productCode,
      quantity: quantity
    };
    orderLines = orderLines.concat(orderLine);
    updateOrder();
  }

  function changeShippingSameAsAddress() {
    if (shippingAddressSameAsAddress === true) {
      shippingAddress = billingAddress;
    }
  }

  async function addressChange(event) {
    billingAddress = event.detail;
    changeShippingSameAsAddress();
    updateOrder();
  }

  // function shippingAddressChange(event) {
  //   shippingAddress = event.detail;
  //   console.log("shippingAddressChange " + JSON.stringify(shippingAddress));
  // }

  async function customerChange(event) {
    customer = event.detail;
    updateOrder();
  }
</script>

<div>
  <h1>Order Taking</h1>
  <Customer on:customerChange={customerChange}/>
  <Address on:addressChange={addressChange}/>
  <!-- <label for="shippingAddressSameAsAddress">Same as billingAddress.</label>
  <input id="shippingAddressSameAsAddress" type="checkbox" bind:value="{shippingAddressSameAsAddress}"/>
  {#if (shippingAddressSameAsAddress === false)}
    <h2>Shipping billingAddress</h2>
    <Address on:addressChange={shippingAddressChange}/>
  {/if} -->
  <div>
    <label for="productCode">Product code:</label>
    <input id="productCode" type="text" bind:value="{productCode}"/>
    <label for="quantity">Quantity:</label>
    <input id="quantity" type="text" bind:value="{quantity}"/>
    <Button on:click="{addOrderLine}">Add product</Button>
  </div>
  {#if orderLines.length === 0}
    <p>No orders were added yet.</p>
  {:else}
    {#each orderLines as orderLine}
      <OrderLine productCode={orderLine.productCode} quantity={orderLine.quantity}/>
    {/each}
  {/if}
  <Button on:click="{submitOrder}">Submit Order</Button>
</div>
