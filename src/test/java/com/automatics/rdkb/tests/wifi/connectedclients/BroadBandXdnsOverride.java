/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package com.automatics.rdkb.tests.wifi.connectedclients;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.core.SupportedModelHandler;
import com.automatics.device.Dut;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;

public class BroadBandXdnsOverride extends AutomaticsTestBase{
	
    /**
     * 
     * <ol>
     * <li>1 :Connect to a connected client with 2.4Ghz/5Ghz radio ssid and verify connection status</li>
     * <li>2 :Execute nslookup facebook.com Wifi connected client and verify output</li>
     * <li>3 :Execute nslookup gmail.com 8.8.8.8 Wifi connected client and verify output</li>
     * <li>4 :Execute nslookup amazon.com 75.75.76.76 Wifi connected client and verify output</li>
     * <li>5 :Execute nslookup opendns.com 208.67.222.222 Wifi connected client and verify output</li>
     * <li>6 :Execute nslookup yahoo.com 208.67.220.220 Wifi connected client and verify output</li>
     * <li>7 :Execute nslookup comcast.com 208.67.222.220 Wifi connected client and verify output</li>
     * <li>8 :Execute nslookup cisco.com 208.67.220.222 Wifi connected client and verify output</li>
     * <li>9 :Retrieve ethernet client from connected clients</li>
     * <li>10 :Execute nslookup facebook.com in ethernet connected client and verify output</li>
     * <li>11 :Execute nslookup gmail.com 8.8.8.8 ethernet connected client and verify output</li>
     * <li>12 :Execute nslookup amazon.com 75.75.76.76 ethernet connected client and verify output</li>
     * <li>13 :Execute nslookup opendns.com 208.67.222.222 ethernet connected client and verify output</li>
     * <li>14 :Execute nslookup yahoo.com 208.67.220.220 ethernet connected client and verify output</li>
     * <li>15 :Execute nslookup comcast.com 208.67.222.220 ethernet connected client and verify output</li>
     * <li>16 :Execute nslookup cisco.com 208.67.220.222 ethernet connected client and verify output</li>
     * </ol>
     * 
     * @author PRASANTH REDDY
     * @Refactor Alan_Bivera
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, groups = {
    		BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-ALTN-DNS-1001")
    public void testToVerifyAlternateDns(Dut device) {
	boolean status = false;// String to store the test case status
	String testId = "TC-RDKB-ALTN-DNS-101";// Test case id
	String testStep = null;// Test step number
	String errorMessage = null;// String to store the error message
	Dut wifiConnectedClient = null;
	String openDnsIP = null; // String to store dns ip
	try {
	    LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-ALTN-DNS-1001#####################");
	    LOGGER.info("TEST DESCRIPTION: Test for Alternate DNS in WiFi and Lan Connected clients");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("PRE-CONDITION 1: Verify whether Private SSID 2.4Ghz can be enabled using webPA");
	    LOGGER.info("PRE-CONDITION 2: Verify whether Private SSID 5Ghz can be enabled using webPA");
	    LOGGER.info("1.Connect to a connected client with 2.4Ghz/5Ghz radio ssid and verify connection status");
	    LOGGER.info("2.Execute nslookup facebook.com Wifi connected client and verify output");
	    LOGGER.info("3.Execute nslookup gmail.com 8.8.8.8 Wifi connected client and verify output");
	    LOGGER.info("4.Execute nslookup amazon.com 75.75.76.76 Wifi connected client and verify output");
	    LOGGER.info("5.Execute nslookup opendns.com 208.67.222.222 Wifi connected client and verify output");
	    LOGGER.info("6.Execute nslookup yahoo.com 208.67.220.220 Wifi connected client and verify output");
	    LOGGER.info("7.Execute nslookup comcast.com 208.67.222.220 Wifi connected client and verify output");
	    LOGGER.info("8.Execute nslookup cisco.com 208.67.220.222 Wifi connected client and verify output");
	    LOGGER.info("9.Retrieve ethernet client from connected clients");
	    LOGGER.info("10.Execute nslookup facebook.com  in ethernet connected client and verify output");
	    LOGGER.info("11.Execute nslookup gmail.com 8.8.8.8 ethernet connected client and verify output");
	    LOGGER.info("12.Execute nslookup amazon.com 75.75.76.76 ethernet connected client and verify output");
	    LOGGER.info("13.Execute nslookup opendns.com 208.67.222.222 ethernet connected client and verify output");
	    LOGGER.info("14.Execute nslookup yahoo.com 208.67.220.220 ethernet connected client and verify output");
	    LOGGER.info("15.Execute nslookup comcast.com 208.67.222.220 ethernet connected client and verify output");
	    LOGGER.info("16.Execute nslookup cisco.com 208.67.220.222 ethernet connected client and verify output");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("############################# STARTING PRE-CONFIGURATIONS ##################################");
	    LOGGER.info("PRE-CONFIGURATIONS TEST STEPS : ");
	    LOGGER.info("1.Verify whether Private SSID 2.4Ghz can be enabled using webPA");
	    LOGGER.info("2.Verify whether Private SSID 5Ghz can be enabled using webPA");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION 1: VERIFY WHETHER PRIVATE SSID 2.4GHZ CAN BE ENABLED USING WEBPA");
	    LOGGER.info("PRE-CONDITION 1: EXPECTED: PRIVATE SSID 2.4GHZ SHOULD BE ENABLED SUCCESSFULLY");
	    errorMessage = "Unable to enable 2.4Ghz private SSID using webpa";
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE,
		    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (!status) {
		throw new Exception(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION 2: VERIFY WHETHER PRIVATE SSID 5GHZ CAN BE ENABLED USING WEBPA");
	    LOGGER.info("PRE-CONDITION 2: EXPECTED: PRIVATE SSID 5GHZ SHOULD BE ENABLED SUCCESSFULLY");
	    errorMessage = "Unable to enable 5Ghz private SSID using webpa";
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE,
		    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (!status) {
		throw new Exception(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }
	    /**
	     * STEP 1:Connect to a connected client with 2.4Ghz/5Ghz radio ssid and verify connection status
	     */
	    status = false;
	    testStep = "s1";
	    LOGGER.info("##########################################################################");
	    LOGGER.info(
		    "STEP 1 : DESCRIPTION :CONNECT TO A CONNECTED CLIENT WITH 2.4GHZ/5GHZ RADIO SSID AND VERIFY CONNECTION STATUS");
	    LOGGER.info(
		    "STEP 1 : ACTION :CONNECTION ESTABLISHMENT BETWEEN CONNECTED CLIENT -2.4GHZ/5GHZ AND DEVICE SHOULD BE SUCCESSFUL");
	    LOGGER.info("STEP 1 : EXPECTED:THE CONNECTION SHOULD BE SUCCESSFUL TO 2.4GHZ/5GHZ RADIO SSID");
	    errorMessage = "Unable to establish wifi connection in connected client";
	    try {
		wifiConnectedClient = BroadBandConnectedClientUtils
			.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
	    } catch (Exception e) {
		LOGGER.error("Exception in retrieving wifi connected client");
	    }
	    status = wifiConnectedClient != null;
	    if (status) {
		LOGGER.info("STEP 1:ACTUAL :Connected to Wifi SSID successfully");
	    } else {
		LOGGER.error("STEP 1:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);
	    /**
	     * STEP 2:Execute nslookup facebook.com Wifi connected client and verify output
	     */
	    status = false;
	    testStep = "s2";
	    LOGGER.info("##########################################################################");
	    LOGGER.info("STEP 2 : DESCRIPTION :EXECUTE nslookup facebook.com WIFI CONNECTED CLIENT AND VERIFY OUTPUT");
	    LOGGER.info("STEP 2 : ACTION :EXECUTE COMMAND 'nslookup facebook.com' IN WIFI CONNECTED CLIENT");
	    LOGGER.info("STEP 2 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
	    errorMessage = "Unable to validate nslookup response in wifi connected client";
	    status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
		    wifiConnectedClient, BroadBandTestConstants.NSLOOKUP_FOR_FACEBOOK, "",
		    BroadBandTestConstants.NSLOOKUP_FOR_FACEBOOK);
	    if (status) {
		LOGGER.info("STEP 2:ACTUAL :Nslookup validation is successful");
	    } else {
		LOGGER.error("STEP 2:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
	    /**
	     * STEP 3:Execute nslookup gmail.com 8.8.8.8 Wifi connected client and verify output
	     */
	    status = false;
	    testStep = "s3";
	    LOGGER.info("##########################################################################");
	    LOGGER.info(
		    "STEP 3 : DESCRIPTION :EXECUTE nslookup gmail.com 8.8.8.8 WIFI CONNECTED CLIENT AND VERIFY OUTPUT");
	    LOGGER.info("STEP 3 : ACTION :EXECUTE COMMAND 'nslookup gmail.com 8.8.8.8' IN WIFI CONNECTED CLIENT");
	    LOGGER.info("STEP 3 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
	    errorMessage = "Unable to validate nslookup with DNS 8.8.8.8 response in wifi connected client";
	    status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
		    wifiConnectedClient, BroadBandTestConstants.NSLOOKUP_FOR_GMAIL,
		    BroadBandTestConstants.STRING_OPEN_DNS_IP_8, BroadBandTestConstants.NSLOOKUP_FOR_GMAIL);
	    if (status) {
		LOGGER.info("STEP 3:ACTUAL :Nslookup validation is successful with DNS 8.8.8.8");
	    } else {
		LOGGER.error("STEP 3:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
	    /**
	     * STEP 4:Execute nslookup amazon.com 75.75.76.76 Wifi connected client and verify output
	     */
	    status = false;
	    testStep = "s4";
	    openDnsIP = DeviceModeHandler.isDSLDevice(device)
		    ? BroadBandTestConstants.STRING_OPEN_DNS_IP_DSL
		    : BroadBandTestConstants.STRING_OPEN_DNS_IP_75_76;
	    LOGGER.info("##########################################################################");
	    LOGGER.info("STEP 4 : DESCRIPTION :EXECUTE nslookup amazon.com " + openDnsIP
		    + " WIFI CONNECTED CLIENT AND VERIFY OUTPUT");
	    LOGGER.info("STEP 4 : ACTION :EXECUTE COMMAND 'nslookup amazon.com " + openDnsIP
		    + " ' IN WIFI CONNECTED CLIENT");
	    LOGGER.info("STEP 4 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
	    errorMessage = "Unable to validate nslookup with DNS " + openDnsIP + " response in wifi connected client";

	    status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
		    wifiConnectedClient, BroadBandTestConstants.NSLOOKUP_FOR_AMAZON, openDnsIP,
		    BroadBandTestConstants.NSLOOKUP_FOR_AMAZON);
	    if (status) {
		LOGGER.info("STEP 4:ACTUAL :Nslookup validation is successful with DNS 75.75.76.76");
	    } else {
		LOGGER.error("STEP 4:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
	    /**
	     * STEP 5:Execute nslookup opendns.com 208.67.222.222 Wifi connected client and verify output
	     */
	    status = false;
	    testStep = "s5";
	    LOGGER.info("##########################################################################");
	    LOGGER.info(
		    "STEP 5 : DESCRIPTION :EXECUTE nslookup opendns.com 208.67.222.222 WIFI CONNECTED CLIENT AND VERIFY OUTPUT");
	    LOGGER.info(
		    "STEP 5 : ACTION :EXECUTE COMMAND 'nslookup opendns.com 208.67.222.222' IN WIFI CONNECTED CLIENT");
	    LOGGER.info("STEP 5 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
	    errorMessage = "Unable to validate nslookup with DNS 208.67.222.222 response in wifi connected client";
	    status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
		    wifiConnectedClient, BroadBandTestConstants.NSLOOKUP_FOR_OPENDNS,
		    BroadBandTestConstants.STRING_OPEN_DNS_IP_222, BroadBandTestConstants.NSLOOKUP_FOR_OPENDNS);
	    if (status) {
		LOGGER.info("STEP 5:ACTUAL :Nslookup validation is successful with DNS 208.67.222.222");
	    } else {
		LOGGER.error("STEP 5:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
	    /**
	     * STEP 6:Execute nslookup yahoo.com 208.67.220.220 Wifi connected client and verify output
	     */
	    status = false;
	    testStep = "s6";
	    LOGGER.info("##########################################################################");
	    LOGGER.info(
		    "STEP 6 : DESCRIPTION :EXECUTE nslookup yahoo.com 208.67.220.220 WIFI CONNECTED CLIENT AND VERIFY OUTPUT");
	    LOGGER.info(
		    "STEP 6 : ACTION :EXECUTE COMMAND 'nslookup yahoo.com 208.67.220.220' IN WIFI CONNECTED CLIENT");
	    LOGGER.info("STEP 6 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
	    errorMessage = "Unable to validate nslookup with DNS 208.67.220.220 response in wifi connected client";
	    status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
		    wifiConnectedClient, BroadBandTestConstants.NSLOOKUP_FOR_YAHOO,
		    BroadBandTestConstants.STRING_OPEN_DNS_IP_220, BroadBandTestConstants.NSLOOKUP_FOR_YAHOO);
	    if (status) {
		LOGGER.info("STEP 6:ACTUAL :Nslookup validation is successful with DNS 208.67.220.220");
	    } else {
		LOGGER.error("STEP 6:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
	    /**
	     * STEP 7:Execute nslookup comcast.com 208.67.222.220 Wifi connected client and verify output
	     */
	    status = false;
	    testStep = "s7";
	    LOGGER.info("##########################################################################");
	    LOGGER.info(
		    "STEP 7 : DESCRIPTION :EXECUTE nslookup comcast.com 208.67.222.220 WIFI CONNECTED CLIENT AND VERIFY OUTPUT");
	    LOGGER.info(
		    "STEP 7 : ACTION :EXECUTE COMMAND 'nslookup comcast.com 208.67.222.220' IN WIFI CONNECTED CLIENT");
	    LOGGER.info("STEP 7 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
	    errorMessage = "Unable to validate nslookup with DNS 208.67.222.220 response in wifi connected client";
	    status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
		    wifiConnectedClient, BroadBandTestConstants.NSLOOKUP_FOR_COMCAST,
		    BroadBandTestConstants.STRING_OPEN_DNS_IP_222_220, BroadBandTestConstants.NSLOOKUP_FOR_COMCAST);
	    if (status) {
		LOGGER.info("STEP 7:ACTUAL :Nslookup validation is successful with DNS 208.67.222.220");
	    } else {
		LOGGER.error("STEP 7:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
	    /**
	     * STEP 8:Execute nslookup cisco.com 208.67.220.222 Wifi connected client and verify output
	     */
	    status = false;
	    testStep = "s8";
	    LOGGER.info("##########################################################################");
	    LOGGER.info(
		    "STEP 8 : DESCRIPTION :EXECUTE nslookup cisco.com 208.67.220.222 WIFI CONNECTED CLIENT AND VERIFY OUTPUT");
	    LOGGER.info(
		    "STEP 8 : ACTION :EXECUTE COMMAND 'nslookup cisco.com 208.67.220.222' IN WIFI CONNECTED CLIENT");
	    LOGGER.info("STEP 8 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
	    errorMessage = "Unable to validate nslookup with DNS 208.67.220.222 response in wifi connected client";
	    status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
		    wifiConnectedClient, BroadBandTestConstants.NSLOOKUP_FOR_CISCO,
		    BroadBandTestConstants.STRING_OPEN_DNS_IP_220_222, BroadBandTestConstants.NSLOOKUP_FOR_CISCO);
	    if (status) {
		LOGGER.info("STEP 8:ACTUAL :Nslookup validation is successful with DNS 208.67.220.222");
	    } else {
		LOGGER.error("STEP 8:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
	    /**
	     * STEP 9:Retrieve ethernet client from connected clients
	     */
	    status = false;
	    testStep = "s9";
	    LOGGER.info("##########################################################################");
	    LOGGER.info("STEP 9 : DESCRIPTION :RETRIEVE ETHERNET CLIENT FROM CONNECTED CLIENTS");
	    LOGGER.info("STEP 9 : ACTION :ETHERNET CLIENT SHOULD BE RETRIEVED SUCCESSFULLY");
	    LOGGER.info("STEP 9 : EXPECTED:ETHERENT RETRIEVED SUCCESSFULLY");
	    errorMessage = "Unable to validate nslookup response in wifi connected client";
	    Dut ethernetClient = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
	    status = ethernetClient != null;
	    if (status) {
		LOGGER.info("STEP 9:ACTUAL :Ethernet connected is retrieved successfully");
	    } else {
		LOGGER.error("STEP 9:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);
	    /**
	     * STEP 10:Execute nslookup facebook.com in ethernet connected client and verify output
	     */
	    status = false;
	    testStep = "s10";
	    LOGGER.info("##########################################################################");
	    LOGGER.info(
		    "STEP 10 : DESCRIPTION :EXECUTE nslookup facebook.com  IN ETHERNET CONNECTED CLIENT AND VERIFY OUTPUT");
	    LOGGER.info("STEP 10 : ACTION :EXECUTE COMMAND 'nslookup facebook.com' IN ETHERNET CONNECTED CLIENT");
	    LOGGER.info("STEP 10 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
	    errorMessage = "Unable to validate nslookup response in wifi connected client";
	    status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
		    ethernetClient, BroadBandTestConstants.NSLOOKUP_FOR_FACEBOOK, "",
		    BroadBandTestConstants.NSLOOKUP_FOR_FACEBOOK);
	    if (status) {
		LOGGER.info("STEP 10:ACTUAL :Nslookup validation is successful");
	    } else {
		LOGGER.error("STEP 10:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
	    /**
	     * STEP 11:Execute nslookup gmail.com 8.8.8.8 ethernet connected client and verify output
	     */
	    status = false;
	    testStep = "s11";
	    LOGGER.info("##########################################################################");
	    LOGGER.info(
		    "STEP 11 : DESCRIPTION :EXECUTE nslookup gmail.com 8.8.8.8 IN ETHERNET CONNECTED CLIENT AND VERIFY OUTPUT");
	    LOGGER.info("STEP 11 : ACTION :EXECUTE COMMAND 'nslookup gmail.com 8.8.8.8' IN ETHERNET CONNECTED CLIENT");
	    LOGGER.info("STEP 11 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
	    errorMessage = "Unable to validate nslookup with DNS 8.8.8.8 response in wifi connected client";
	    status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
		    ethernetClient, BroadBandTestConstants.NSLOOKUP_FOR_GMAIL,
		    BroadBandTestConstants.STRING_OPEN_DNS_IP_8, BroadBandTestConstants.NSLOOKUP_FOR_GMAIL);
	    if (status) {
		LOGGER.info("STEP 11:ACTUAL :Nslookup validation is successful with DNS 8.8.8.8");
	    } else {
		LOGGER.error("STEP 11:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
	    /**
	     * STEP 12:Execute nslookup amazon.com 75.75.76.76 ethernet connected client and verify output
	     */
	    status = false;
	    testStep = "s12";
	    LOGGER.info("##########################################################################");
	    LOGGER.info("STEP 12 : DESCRIPTION :EXECUTE nslookup amazon.com " + openDnsIP
		    + " IN ETHERNET CONNECTED CLIENT AND VERIFY OUTPUT");
	    LOGGER.info("STEP 12 : ACTION :EXECUTE COMMAND 'nslookup amazon.com " + openDnsIP
		    + "' IN ETHERNET CONNECTED CLIENT");
	    LOGGER.info("STEP 12 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
	    errorMessage = "Unable to validate nslookup with DNS " + openDnsIP + " response in wifi connected client";
	    status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
		    ethernetClient, BroadBandTestConstants.NSLOOKUP_FOR_AMAZON, openDnsIP,
		    BroadBandTestConstants.NSLOOKUP_FOR_AMAZON);
	    if (status) {
		LOGGER.info("STEP 12:ACTUAL :Nslookup validation is successful with DNS " + openDnsIP);
	    } else {
		LOGGER.error("STEP 12:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
	    /**
	     * STEP 13:Execute nslookup opendns.com 208.67.222.222 ethernet connected client and verify output
	     */
	    status = false;
	    testStep = "s13";
	    LOGGER.info("##########################################################################");
	    LOGGER.info(
		    "STEP 13 : DESCRIPTION :EXECUTE nslookup opendns.com 208.67.222.222 IN ETHERNET CONNECTED CLIENT AND VERIFY OUTPUT");
	    LOGGER.info(
		    "STEP 13 : ACTION :EXECUTE COMMAND 'nslookup opendns.com 208.67.222.222' IN ETHERNET CONNECTED CLIENT");
	    LOGGER.info("STEP 13 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
	    errorMessage = "Unable to validate nslookup with DNS 208.67.222.222 response in wifi connected client";
	    status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
		    ethernetClient, BroadBandTestConstants.NSLOOKUP_FOR_OPENDNS,
		    BroadBandTestConstants.STRING_OPEN_DNS_IP_222, BroadBandTestConstants.NSLOOKUP_FOR_OPENDNS);
	    if (status) {
		LOGGER.info("STEP 13:ACTUAL :Nslookup validation is successful with DNS 208.67.222.222");
	    } else {
		LOGGER.error("STEP 13:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
	    /**
	     * STEP 14:Execute nslookup yahoo.com 208.67.220.220 ethernet connected client and verify output
	     */
	    status = false;
	    testStep = "s14";
	    LOGGER.info("##########################################################################");
	    LOGGER.info(
		    "STEP 14 : DESCRIPTION :EXECUTE nslookup yahoo.com 208.67.220.220 IN ETHERNET CONNECTED CLIENT AND VERIFY OUTPUT");
	    LOGGER.info(
		    "STEP 14 : ACTION :EXECUTE COMMAND 'nslookup yahoo.com 208.67.220.220' IN ETHERNET CONNECTED CLIENT");
	    LOGGER.info("STEP 14 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
	    errorMessage = "Unable to validate nslookup with DNS 208.67.220.220 response in wifi connected client";
	    status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
		    ethernetClient, BroadBandTestConstants.NSLOOKUP_FOR_YAHOO,
		    BroadBandTestConstants.STRING_OPEN_DNS_IP_220, BroadBandTestConstants.NSLOOKUP_FOR_YAHOO);
	    if (status) {
		LOGGER.info("STEP 14:ACTUAL :Nslookup validation is successful with DNS 208.67.220.220");
	    } else {
		LOGGER.error("STEP 14:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
	    /**
	     * STEP 15:Execute nslookup comcast.com 208.67.222.220 ethernet connected client and verify output
	     */
	    status = false;
	    testStep = "s15";
	    LOGGER.info("##########################################################################");
	    LOGGER.info(
		    "STEP 15 : DESCRIPTION :EXECUTE nslookup comcast.com 208.67.222.220 IN ETHERNET CONNECTED CLIENT AND VERIFY OUTPUT");
	    LOGGER.info(
		    "STEP 15 : ACTION :EXECUTE COMMAND 'nslookup comcast.com 208.67.222.220' IN ETHERNET CONNECTED CLIENT");
	    LOGGER.info("STEP 15 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
	    errorMessage = "Unable to validate nslookup with DNS 208.67.222.220 response in wifi connected client";
	    status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
		    ethernetClient, BroadBandTestConstants.NSLOOKUP_FOR_COMCAST,
		    BroadBandTestConstants.STRING_OPEN_DNS_IP_222_220, BroadBandTestConstants.NSLOOKUP_FOR_COMCAST);
	    if (status) {
		LOGGER.info("STEP 15:ACTUAL :Nslookup validation is successful with DNS 208.67.222.220");
	    } else {
		LOGGER.error("STEP 15:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
	    /**
	     * STEP 16:Execute nslookup cisco.com 208.67.220.222 ethernet connected client and verify output
	     */
	    status = false;
	    testStep = "s16";
	    LOGGER.info("##########################################################################");
	    LOGGER.info(
		    "STEP 16 : DESCRIPTION :EXECUTE nslookup cisco.com 208.67.220.222 IN ETHERNET CONNECTED CLIENT AND VERIFY OUTPUT");
	    LOGGER.info(
		    "STEP 16 : ACTION :EXECUTE COMMAND 'nslookup cisco.com 208.67.220.222' IN ETHERNET CONNECTED CLIENT");
	    LOGGER.info("STEP 16 : EXPECTED:COMMAND SHOULD BE EXECUTED SUCCESSFULLY");
	    errorMessage = "Unable to validate nslookup with DNS 208.67.220.222 response in wifi connected client";
	    status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
		    ethernetClient, BroadBandTestConstants.NSLOOKUP_FOR_CISCO,
		    BroadBandTestConstants.STRING_OPEN_DNS_IP_220_222, BroadBandTestConstants.NSLOOKUP_FOR_CISCO);
	    if (status) {
		LOGGER.info("STEP 16:ACTUAL :Nslookup validation is successful with DNS 208.67.220.222");
	    } else {
		LOGGER.error("STEP 16:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception in Validating Alternate DNS \n" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStep, status, errorMessage, true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("POST-CONDITION 1 : DESCRIPTION : Disconnect Wifi Radio 2.4Ghz/5Ghz SSID from the device");
	    LOGGER.info("POST-CONDITION 1 : ACTION :Disconnect wifi radio 2.4Ghz/5Ghz SSID ");
	    LOGGER.info(
		    "POST-CONDITION 1 : EXPECTED : Wifi radio 2.4Ghz/5Ghz SSID should be disconnected successfully");
	    LOGGER.info("#######################################################################################");
	    try {
		BroadBandResultObject bandResultObject = BroadBandConnectedClientUtils
			.disconnectCnnClientFromSsid(tapEnv, device, wifiConnectedClient);
		LOGGER.info("POST CONDITION 1:ACTUAL: WIFI SSID 2.4GHZ/5GHZ Disconnect status: "
			+ bandResultObject.isStatus());
	    } catch (Exception exception2) {
		LOGGER.error("EXCEPTION OCCURRED WHILE PERFORMING POST CONFIGURATION 1" + exception2.getMessage());
	    }
	    LOGGER.info("########################### ENDING POST CONFIGURATION ####################################");

	}
	LOGGER.info("#################### ENDING TEST CASE: TC-RDKB-ALTN-DNS-1001#####################");
    }

}
