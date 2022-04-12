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
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandConnectedClientTestConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandXdnsOverride extends AutomaticsTestBase {

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
	    openDnsIP = DeviceModeHandler.isDSLDevice(device) ? BroadBandTestConstants.STRING_OPEN_DNS_IP_DSL
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

    /**
     * @TestDetails To apply and Verify XDNS ,override with different cases of Invalid values of MAC and DNS IPs.
     * 
     * 
     *              STEP 1 : Enable and verify the XDNS feature using webpa param
     *              Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS
     *              <li>EXPECTED: XDNS feature should be enabled/disabled using webpa param
     *              'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'
     *
     *              STEP 2 : Obtain a working Connected client associated with the device
     *              <li>EXPECTED: Connected client should be obtained successfully
     *
     *              STEP 3 : Verify if connected client is able to access the internet by pinging the site :
     *              'www.google.com'
     *              <li>EXPECTED: Connected client should be able to access the site : 'www.google.com'
     *
     *              STEP 4 : Verify the presence of the dnsmasq_servers.conf file in the device and Applying xdns
     *              override by adding DNS mapping table add DNS mapping table with values MacAddress : Wi-Fi MacAddress
     *              of the connected client , DnsIPv4 : <IPv4> , DnsIPv6 : <IPv6>
     *              <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table should be added
     *              successfully.
     *
     *              STEP 5 : Verify xdns override at client mac Level by validating the domain name of the site :
     *              www.seriesw.net on the connected client
     *              <li>EXPECTED: Connected client should be blocked from accessing the site : www.seriesw.net
     *
     *              STEP 6 : Factory Reset the device and verify if device comes up
     *              <li>EXPECTED: Device should come up after Factory reset
     *
     *              STEP 7 : Set and verify the Global DNS IPv4 value to <IPv4> and Global DNS IPv6 value to <IPv6>
     *              <li>EXPECTED: Global DNS IPv4 and IPv6 values should be set to <IPv4> and <IPv6> respectively.
     * 
     *              STEP 8 : Enable and verify the XDNS feature using webpa param
     *              Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS
     *              <li>EXPECTED: XDNS feature should be enabled/disabled using webpa param
     *              'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'
     * 
     *              STEP 9 : Verify Captive portal status of the router after factory reset and connect the client
     *              device to the SSID after reactivating the router.
     *              <li>EXPECTED: Device should be in Captive portal mode after factory reset and the connected clients
     *              should able to access Network after reactivating the router
     *
     *              STEP 10 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping
     *              table with values MacAddress : Wi-Fi MacAddress of the client , DnsIPv4 : <IPv4>, DnsIPv6 : <IPv6>
     *              <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table should fail to add with
     *              the given Invalid values of MacAddress/DnsIPv4.
     *
     *              STEP 11 : Verify xdns override at client mac Level by validating the domain name of the site :
     *              www.seriesw.net after applying dns override with Invalid macaddress/dnsserver values
     *              <li>EXPECTED: The response should have 'www.seriesw.net' as the domain name for the site :
     *              www.seriesw.net
     *
     *              STEP 12 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping
     *              table with values MacAddress : A:B:C:X:Y:Z , DnsIPv4 : <IPv4>, DnsIPv6 : <IPv6>
     *              <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table should fail to add with
     *              the given Invalid values of MacAddress/DnsIPv4.
     *
     *              STEP 13 : Verify xdns override at client mac Level by validating the domain name of the site :
     *              www.seriesw.net after applying dns override with Invalid macaddress/dnsserver values
     *              <li>EXPECTED: The response should have <site> as the domain name for the site : www.seriesw.net
     *
     *              STEP 14 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping
     *              table with values MacAddress : A:B:C:X:Y:Z , DnsIPv4 : <IPv4>, DnsIPv6 : <IPv6>
     *              <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table should fail to add with
     *              the given Invalid values of MacAddress/DnsIPv4.
     *
     *              STEP 15 : Verify xdns override at client mac Level by validating the domain name of the site :
     *              www.seriesw.net after applying dns override with Invalid macaddress/dnsserver values
     *              <li>EXPECTED: The response should have 'www.seriesw.net' as the domain name for the site :
     *              www.seriesw.net
     *
     *              STEP 16 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping
     *              table with values MacAddress : A:B:C:X:Y:Z , DnsIPv4 : <IPv4>, DnsIPv6 : <IPv6>
     *              <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table should fail to add with
     *              the given Invalid values of MacAddress/DnsIPv4.
     *
     *              STEP 17 : Verify xdns override at client mac Level by validating the domain name of the site :
     *              www.seriesw.net after applying dns override with Invalid macaddress/dnsserver values
     *              <li>EXPECTED: The response should have 'www.seriesw.net' as the domain name for the site :
     *              www.seriesw.net
     *
     *              STEP 18 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping
     *              table with values MacAddress : A:B:C:X:Y:Z , DnsIPv4 : <IPv4>, DnsIPv6 : <IPv6>
     *              <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table should fail to add with
     *              the given Invalid values of MacAddress/DnsIPv4.
     *
     *              STEP 19 : Verify xdns override at client mac Level by validating the domain name of the site :
     *              www.seriesw.net after applying dns override with Invalid macaddress/dnsserver values
     *              <li>EXPECTED: The response should have <site> as the domain name for the site : www.seriesw.net
     *
     *              STEP 20 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping
     *              table with values MacAddress : A:B:C:X:Y:Z, DnsIPv4 : <IPv4>, DnsIPv6 : <IPv6>
     *              <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table should fail to add with
     *              the given Invalid values of MacAddress/DnsIPv4.
     *
     *              STEP 21 : Verify xdns override at client mac Level by validating the domain name of the site :
     *              www.seriesw.net after applying dns override with Invalid macaddress/dnsserver values
     *              <li>EXPECTED: The response should have <site> as the domain name for the site : www.seriesw.net
     *
     *              STEP 22 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping
     *              table with values MacAddress : A:B:C:X:Y:Z , DnsIPv4 : <IPv4>, DnsIPv6 : <IPv6>
     *              <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table should fail to add with
     *              the given Invalid values of MacAddress/DnsIPv4.
     *
     *              STEP 23 : Verify xdns override at client mac Level by validating the domain name of the site :
     *              www.seriesw.net after applying dns override with Invalid macaddress/dnsserver values
     *              <li>EXPECTED: The response should have 'www.seriesw.net' as the domain name for the site :
     *              www.seriesw.net
     *
     *              STEP 24 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping
     *              table with values MacAddress : A:B:C:X:Y:Z , DnsIPv4 : Wi-Fi MacAddress of the client, DnsIPv6 :
     *              <IPv6>
     *              <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table should fail to add with
     *              the given Invalid values of MacAddress/DnsIPv4.
     *
     *              STEP 25 : Verify xdns override at client mac Level by validating the domain name of the site :
     *              www.seriesw.net after applying dns override with Invalid macaddress/dnsserver values
     *              <li>EXPECTED: The response should have <site> as the domain name for the site : www.seriesw.net
     * 
     * 
     * 
     * @author Susheela C
     * @refactor yamini.s
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-XDNS-1004")
    public void applyAndVerifyInvalidValuesForMACAndDnsIPAreNotAcceptedForDnsoverride(Dut device) {
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-XDNS-1004");
	LOGGER.info(
		"TEST DESCRIPTION: To apply and Verify XDNS override with different cases of Invalid values of MAC and DNS IPs");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info(
		"STEP 1 : Enable and verify the XDNS feature using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS");
	LOGGER.info("STEP 2 : Obtain a working Connected client associated with the device");
	LOGGER.info(
		"STEP 3 : Verify if connected client is able to access the internet by pinging the site : 'www.google.com'");

	LOGGER.info(
		"STEP 4 : Verify the presence of the dnsmasq_servers.conf file in the device and Applying xdns override by adding DNS mapping table add DNS mapping table with values MacAddress : Wi-Fi MacAddress of the connected client ");
	LOGGER.info(
		"STEP 5 : Verify xdns override at client mac Level by Accessing site : www.seriesw.net on the connected client");
	LOGGER.info("STEP 6 : Factory Reset the device and verify if device comes up");
	LOGGER.info(
		"STEP 7 : Set and verify the Global DNS IPv4 value to '<IPv4>' and Global DNS IPv6 value to '<IPv6>'");
	LOGGER.info(
		"STEP 8 : Enable and verify the XDNS feature using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS");

	LOGGER.info(
		"STEP 9 : Verify Captive portal status of the router after factory reset and connect the client device to the SSID after reactivating the router.");

	LOGGER.info(
		"STEP 10 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table with values MacAddress : A:B:C:X:Y:Z , DnsIPv4, DnsIPv6");
	LOGGER.info(
		"STEP 11 : Verify xdns override at client mac Level by validating the domain name of the site : <site> after applying dns override with Invalid macaddress/dnsserver values");
	LOGGER.info(
		"STEP 12 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table with values MacAddress : A:B:C:X:Y:Z, DnsIPv4, DnsIPv6");
	LOGGER.info(
		"STEP 13 : Verify xdns override at client mac Level by validating the domain name of the site : <site> after applying dns override with Invalid macaddress/dnsserver values");
	LOGGER.info(
		"STEP 14 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table with values MacAddress : A:B:C:X:Y:Z , DnsIPv4, DnsIPv6");
	LOGGER.info(
		"STEP 15 : Verify xdns override at client mac Level by validating the domain name of the site : www.seriesw.net after applying dns override with Invalid macaddress/dnsserver values");
	LOGGER.info(
		"STEP 16 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table with values MacAddress : A:B:C:X:Y:Z , DnsIPv4, DnsIPv6 ");
	LOGGER.info(
		"STEP 17 : Verify xdns override at client mac Level by validating the domain name of the site : www.seriesw.net after applying dns override with Invalid macaddress/dnsserver values");
	LOGGER.info(
		"STEP 18 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table with values MacAddress : A:B:C:X:Y:Z , DnsIPv4 : 0.0.0.0, DnsIPv6");
	LOGGER.info(
		"STEP 19 : Verify xdns override at client mac Level by validating the domain name of the site : www.seriesw.net after applying dns override with Invalid macaddress/dnsserver values");
	LOGGER.info(
		"STEP 20 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table with values  MacAddress, DnsIPv4, DnsIPv6");
	LOGGER.info(
		"STEP 21 : Verify xdns override at client mac Level by validating the domain name of the site : www.seriesw.net after applying dns override with Invalid macaddress/dnsserver values");
	LOGGER.info(
		"STEP 22 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table with values MacAddress, DnsIPv4, DnsIPv6");
	LOGGER.info(
		"STEP 23 : Verify xdns override at client mac Level by validating the domain name of the site : www.seriesw.net after applying dns override with Invalid macaddress/dnsserver values");
	LOGGER.info(
		"STEP 24 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table with values  MacAddress, DnsIPv4, DnsIPv6, Wi-Fi MacAddress of the client");
	LOGGER.info(
		"STEP 25 : Verify xdns override at client mac Level by validating the domain name of the site : www.seriesw.net after applying dns override with Invalid macaddress/dnsserver values");
	LOGGER.info("#######################################################################################");
	// Holds the test case ID
	String testCaseId = "TC-RDKB-XDNS-104";
	// boolean variable to store the status
	boolean status = false;
	// Test step number
	String stepNumber = null;
	// Error message
	String errorMessage = null;
	try {
	    /**
	     * PRE-CONDITIONS
	     */
	    executePreConditions(device, testCaseId, true);

	    /**
	     * "s1"
	     */
	    // Test step number
	    stepNumber = "s1";
	    // boolean variable to store the status
	    status = false;
	    setAndVerifyXdnsFeature(device, testCaseId, stepNumber, true);

	    /**
	     * "s2", "s3"
	     */
	    // Test step number
	    stepNumber = "s2";
	    // boolean variable to store the status
	    status = false;
	    Dut connectedClientSettop = verifyConnectedClientAndInternetAccess(device, testCaseId,
		    new String[] { "s2", "s3" });

	    /**
	     * "s4"
	     */
	    // Test step number
	    stepNumber = "s4";
	    // boolean variable to store the status
	    status = false;
	    // Mac address of the connected client
	    String clientMacAddress = ((Device) connectedClientSettop).getConnectedDeviceInfo().getWifiMacAddress();
	    applyDnsoverrideByAddingDNSMappingTable(device,
		    BroadBandTestConstants.STRING_DNS_IPV4_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY,
		    BroadBandTestConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY, clientMacAddress,
		    testCaseId, "s4");
	    /**
	     * "s5"
	     */
	    // Test step number
	    stepNumber = "s5";
	    // boolean variable to store the status
	    status = false;
	    LOGGER.info("Waiting for 10 Seconds...");
	    tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
	    // string to store dns block site address for level one site from
	    // properties file
	    String dnsBlockAddressForLevelOneSite = BroadbandPropertyFileHandler
		    .getPropertyKeyForlevelOneBlockAddress();
	    // string to store site address for level one site from properties
	    // file
	    String addressForLevelOneSite = BroadbandPropertyFileHandler.getPropertyKeyForlevelOneSiteHostAddress();
	    // Connectivity check for Level One site
	    verifySiteAccessInConnectClient(device, connectedClientSettop, addressForLevelOneSite,
		    dnsBlockAddressForLevelOneSite, testCaseId, "s5", false);

	    /**
	     * "s6"
	     */
	    // Test step number
	    stepNumber = "s6";
	    // boolean variable to store the status
	    status = false;
	    executeFactoryResetOnTheDevice(device, testCaseId, stepNumber);
	    /**
	     * "s7"
	     */
	    // Test step number
	    stepNumber = "s7";
	    // boolean variable to store the status
	    status = false;
	    setAndVerifyValuesToGblDNSIPv4IPv6(device, testCaseId, stepNumber);
	    /**
	     * "s8"
	     */
	    // Test step number
	    stepNumber = "s8";
	    // boolean variable to store the status
	    status = false;
	    setAndVerifyXdnsFeature(device, testCaseId, stepNumber, true);

	    /**
	     * "s9"
	     */
	    // Test step number
	    stepNumber = "s9";
	    // boolean variable to store the status
	    status = false;
	    verifyCaptivePrtlModeToReactivateRtrAndConnctClient(device, connectedClientSettop, testCaseId, stepNumber);

	    /**
	     * "s10", "s11"
	     */
	    applyInvalDnsoverrideValsAndVerifySiteNtBlckd(device, connectedClientSettop, addressForLevelOneSite,
		    addressForLevelOneSite, BroadBandTestConstants.STRING_DNS_IPV4_NULL_VALUE,
		    BroadBandTestConstants.STRING_INVALID_VALUE_FOR_MAC_ADDRESS,
		    BroadBandTestConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY, testCaseId,
		    new String[] { "s10", "s11" });

	    /**
	     * "s12", "s13"
	     */
	    applyInvalDnsoverrideValsAndVerifySiteNtBlckd(device, connectedClientSettop, addressForLevelOneSite,
		    addressForLevelOneSite, BroadBandTestConstants.STRING_DNS_IP_WITH_SUBNET_MASK,
		    BroadBandTestConstants.STRING_INVALID_VALUE_FOR_MAC_ADDRESS,
		    BroadBandTestConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY, testCaseId,
		    new String[] { "s12", "s13" });

	    /**
	     * "s14", "s15"
	     */
	    applyInvalDnsoverrideValsAndVerifySiteNtBlckd(device, connectedClientSettop, addressForLevelOneSite,
		    addressForLevelOneSite, BroadBandTestConstants.STRING_DNS_IP_WITH_NETWORK_ADDRESS,
		    BroadBandTestConstants.STRING_INVALID_VALUE_FOR_MAC_ADDRESS,
		    BroadBandTestConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY, testCaseId,
		    new String[] { "s14", "s15" });

	    /**
	     * "s16", "s17"
	     */
	    applyInvalDnsoverrideValsAndVerifySiteNtBlckd(device, connectedClientSettop, addressForLevelOneSite,
		    addressForLevelOneSite, BroadBandTestConstants.STRING_DNS_IP_WITH_BROADCAST_ADDRESS,
		    BroadBandTestConstants.STRING_INVALID_VALUE_FOR_MAC_ADDRESS,
		    BroadBandTestConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY, testCaseId,
		    new String[] { "s16", "s17" });

	    /**
	     * "s18", "s19"
	     */
	    applyInvalDnsoverrideValsAndVerifySiteNtBlckd(device, connectedClientSettop, addressForLevelOneSite,
		    addressForLevelOneSite, BroadBandTestConstants.STRING_DNS_IP_WITH_NULL_BROADCAST_ADDRESS,
		    BroadBandTestConstants.STRING_INVALID_VALUE_FOR_MAC_ADDRESS,
		    BroadBandTestConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY, testCaseId,
		    new String[] { "s18", "s19" });

	    /**
	     * "s20", "s21"
	     */
	    applyInvalDnsoverrideValsAndVerifySiteNtBlckd(device, connectedClientSettop, addressForLevelOneSite,
		    addressForLevelOneSite, BroadBandTestConstants.STRING_DNS_IPV4_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY,
		    BroadBandTestConstants.STRING_DNS_IPV4_VALUE_FOR_DNS_LEVEL_TWO_PRIMARY,
		    BroadBandTestConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY, testCaseId,
		    new String[] { "s20", "s21" });

	    /**
	     * "s22", "s23"
	     */
	    applyInvalDnsoverrideValsAndVerifySiteNtBlckd(device, connectedClientSettop, addressForLevelOneSite,
		    addressForLevelOneSite, BroadBandTestConstants.STRING_DNS_IPV4_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY,
		    BroadBandTestConstants.STRING_INVALID_VALUE_FOR_MAC_ADDRESS,
		    BroadBandTestConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY, testCaseId,
		    new String[] { "s22", "s23" });

	    /**
	     * "s24", "s25"
	     */
	    applyInvalDnsoverrideValsAndVerifySiteNtBlckd(device, connectedClientSettop, addressForLevelOneSite,
		    addressForLevelOneSite, clientMacAddress,
		    BroadBandTestConstants.STRING_DNS_IPV4_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY,
		    BroadBandTestConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY, testCaseId,
		    new String[] { "s24", "s25" });

	} catch (TestException exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("TC-RDKB-XDNS-1004 : Execution error occured due to exception --> " + errorMessage);
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	} finally {
	    BroadBandPostConditionUtils.executePostConditionForXdns(device, tapEnv, true);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
    }

    /**
     * Common step for executing pre-conditions. ie., to set the Global DNS IPV4 value to "<IPv4>" and Global DNS IPV6
     * value to "<IPv6>" and Verify and enable 2.4GHz and 5GHz private wifi connection.
     * 
     * @param device
     * @param testCaseId
     * 
     * @refactor yamini.s
     */
    private void executePreConditions(Dut device, String testCaseId, boolean enablePrivateSsidandReactiveRouter)
	    throws TestException {
	// boolean variable to store the status
	boolean status = false;
	// Error message
	String errorMessage = null;

	LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	LOGGER.info("#######################################################################################");
	LOGGER.info(
		"PRE-CONDITION-1: DESCRIPTION : Set and verify the Global DNS IPv4 value to '<IPv4>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
	LOGGER.info(
		"PRE-CONDITION-1: ACTION : Set the Global DNS IPv4 value to '<IPv4>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
	LOGGER.info(
		"PRE-CONDITION-1: EXPECTED : Global DNS IPv4 value should be set to '<IPv4>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
	LOGGER.info("#######################################################################################");
	status = false;
	status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV4, WebPaDataTypes.STRING.getValue(),
		BroadBandTestConstants.STRING_DEFAULT_GLOBAL_DNS_IPV4_VALUE, BroadBandTestConstants.THREE_MINUTES,
		BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	// Error message
	errorMessage = "Failed to set Global DNS IPv4 value to '<IPv4>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'";
	LOGGER.info("PRE-CONDITION-1: ACTUAL: "
		+ (status ? "Global DNS IPv4 value sucessfully set to '<IPv4>'" : errorMessage));

	if (!status) {
	    throw new TestException(
		    BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION-1 FAILED : " + errorMessage);
	}
	LOGGER.info("#######################################################################################");
	LOGGER.info(
		"PRE-CONDITION-2: DESCRIPTION : Set and verify the Global DNS IPv6 value to '<IPv6>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'");
	LOGGER.info(
		"PRE-CONDITION-2: ACTION : Set the Global DNS IPv6 value to '<IPv6>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'");
	LOGGER.info(
		"PRE-CONDITION-2: EXPECTED : Global DNS IPv6 value should be set to '<IPv6>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'");
	LOGGER.info("#######################################################################################");
	status = false;
	status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV6, WebPaDataTypes.STRING.getValue(),
		BroadBandTestConstants.STRING_DEFAULT_GLOBAL_DNS_IPV6_VALUE, BroadBandTestConstants.THREE_MINUTES,
		BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	// Error message
	errorMessage = "Failed to set Global DNS IPv6 value to '<IPv6>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'";
	LOGGER.info("PRE-CONDITION-2: ACTUAL: "
		+ (status ? "Global DNS IPv6 value sucessfully set to '<IPv6>'" : errorMessage));
	if (!status) {
	    throw new TestException(
		    BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION-2 FAILED : " + errorMessage);
	}
	if (enablePrivateSsidandReactiveRouter) {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "PRE-CONDITION-3: DESCRIPTION : Verify 2.4GHz private SSID status on the Device and Enable using webpa param 'Device.WiFi.SSID.10001.Enable'");
	    LOGGER.info(
		    "PRE-CONDITION-3: ACTION : Get 2.4GHz private SSID status on the Device using webpa param 'Device.WiFi.SSID.10001.Enable'");
	    LOGGER.info(
		    "PRE-CONDITION-3: EXPECTED : 2.4GHz private SSID should be set to enabled using webpa param 'Device.WiFi.SSID.10001.Enable'");
	    LOGGER.info("#######################################################################################");
	    status = false;
	    String successMessage = "The 2.4GHz private SSID on the Device is already enabled.";
	    String response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLE);
	    status = CommonMethods.isNotNull(response)
		    && CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.TRUE);
	    // Error message
	    errorMessage = "Unable get the 2.4GHz private SSID status on this XB Device.";
	    if (!status) {
		// Error message
		errorMessage = "Unable to Enable the 2.4GHz private SSID on this XB Device.";
		status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLE,
			WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE,
			BroadBandTestConstants.THREE_MINUTES, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		successMessage = "The 2.4GHz private SSID on the Device enabled successfully using webpa param 'Device.WiFi.SSID.10001.Enable'.";
	    }

	    LOGGER.info("PRE-CONDITION-3: ACTUAL: " + (status ? successMessage : errorMessage));
	    if (!status) {
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION-3 FAILED : " + errorMessage);
	    }

	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "PRE-CONDITION-4: DESCRIPTION : Verify 5GHz private SSID status on the Device and Enable using webpa param 'Device.WiFi.SSID.10101.Enable'");
	    LOGGER.info(
		    "PRE-CONDITION-4: ACTION : Get 5GHz private SSID status on the Device using webpa param 'Device.WiFi.SSID.10101.Enable'");
	    LOGGER.info(
		    "PRE-CONDITION-4: EXPECTED : 5GHz private SSID should be set to enabled using webpa param 'Device.WiFi.SSID.10101.Enable'");
	    LOGGER.info("#######################################################################################");
	    status = false;
	    successMessage = "The 5GHz private SSID on the Device is already enabled.";
	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS);
	    status = CommonMethods.isNotNull(response)
		    && CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.TRUE);
	    // Error message
	    errorMessage = "Unable get the 5GHz private SSID status on this XB Device.";
	    if (!status) {
		// Error message
		errorMessage = "Unable to Enable the 5GHz private SSID on this XB Device.";
		status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
			WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE,
			BroadBandTestConstants.THREE_MINUTES, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		successMessage = "The 5GHz private SSID on the Device enabled successfully using webpa param 'Device.WiFi.SSID.10101.Enable'.";
	    }
	    LOGGER.info("PRE-CONDITION-4: ACTUAL: " + (status ? successMessage : errorMessage));
	    if (!status) {
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION-4 FAILED : " + errorMessage);
	    }

	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "PRE-CONDITION-5: DESCRIPTION : Re-Activate and verify the device by setting 2.4GHz and 5GHz SSID and Passphrase");
	    LOGGER.info(
		    "PRE-CONDITION-5: ACTION : Re-Activate the device by setting 2.4GHz and 5GHz SSID and Passphrase");
	    LOGGER.info("PRE-CONDITION-5: EXPECTED : Device should be Re-Activated successfully");
	    LOGGER.info("#######################################################################################");
	    status = false;
	    try {
		BroadBandWiFiUtils.reactivateDeviceUsingWebPa(tapEnv, device);
		status = true;
	    } catch (TestException e) {
		errorMessage = e.getMessage();
	    }
	    LOGGER.info(
		    "PRE-CONDITION-5: ACTUAL: " + (status ? "Router Device Re-Activated successfully." : errorMessage));
	    if (!status) {
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION-5 FAILED : " + errorMessage);
	    }
	}
	LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
    }

    /**
     * Common step to set and verify the Xdns Feature using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS
     * 
     * @param dut
     * @param testCaseId
     * @param stepNumber
     * @param valueToSet
     *            boolean value to enable or disable the xdns feature
     * 
     * @refactor yamini.s
     */

    private void setAndVerifyXdnsFeature(Dut device, String testCaseId, String stepNumber, boolean valueToSet) {
	/**
	 * STEP: Enable/Disable and verify the XDNS feature using webpa param
	 * Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS
	 * <li>EXPECTED: XDNS feature should be enabled/disabled using webpa param
	 * 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'
	 */
	// boolean variable to store the status
	boolean status = false;
	// XDNS Value
	String xdnsValue = valueToSet ? BroadBandTestConstants.TRUE : BroadBandTestConstants.FALSE;
	// Error message
	String errorMessage = valueToSet
		? "Failed to enable the XDNS feature using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'"
		: "Failed to disable the XDNS feature using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'";
	// String to store the step description
	String stepDescription = valueToSet
		? "Enable and verify the XDNS feature using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'"
		: "Disable and verify the XDNS feature using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'";
	// String to store the expected result
	String expectedResult = valueToSet
		? "XDNS feature should be enabled using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'"
		: "XDNS feature should be disabled using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'";
	// String to store the success message
	String successMessage = valueToSet
		? "XDNS feature enabled sucessfully using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'"
		: "XDNS feature disabled sucessfully using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'";
	// String to store the action
	String action = valueToSet
		? "Enable the XDNS feature using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'"
		: "Disable the XDNS feature using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STEP :  " + stepNumber + " : DESCRIPTION : " + stepDescription);
	LOGGER.info("STEP :  " + stepNumber + " : ACTION : " + action);
	LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: " + expectedResult);
	LOGGER.info("#######################################################################################");
	status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		BroadBandWebPaConstants.WEBPA_PARAM_TO_GET_XDNS_FEATURE_STATUS, BroadBandTestConstants.CONSTANT_3,
		xdnsValue, BroadBandTestConstants.THREE_MINUTES, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	LOGGER.info("STEP " + stepNumber + " - ACTUAL: " + (status ? successMessage : errorMessage));
	tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, valueToSet);
    }

    /**
     * Common step to get a 2.4GHz/5GHz connected client device associated with the device
     * 
     * @param device
     * @param testCaseId
     * @param stepNumbers
     * 
     * @return connectedClientSettop
     * 
     * @refactor yamini.s
     */

    private Dut verifyConnectedClientAndInternetAccess(Dut device, String testCaseId, String[] stepNumbers) {
	// Connected client instance
	Dut connectedClientSettop = null;
	if (stepNumbers.length == 2) {
	    /**
	     * STEP: Obtain a working Connected client associated with the device
	     * <li>EXPECTED: Connected client should be obtained successfully
	     */
	    // boolean variable to store the status
	    boolean status = false;
	    // Test step number
	    String stepNumber = stepNumbers[0];
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : DESCRIPTION : Obtain a working Connected client associated with the device");
	    LOGGER.info("STEP :  " + stepNumber + " : ACTION : Obtain a working Connected client");
	    LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: Connected client should be obtained successfully");
	    LOGGER.info("#######################################################################################");
	    // Error message
	    String errorMessage = "No working connect client is associated with the device";
	    try {
		connectedClientSettop = BroadBandConnectedClientUtils
			.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
		status = (connectedClientSettop != null);
	    } catch (TestException exeception) {
		// Error message
		errorMessage = exeception.getMessage();
		LOGGER.error(errorMessage);
	    }
	    status = (connectedClientSettop != null);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ":ACTUAL: Connected client associated with the device obtained successfully.");
	    } else {
		LOGGER.error("STEP " + stepNumber + ":ACTUAL: " + errorMessage);

	    }
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "Connected client associated with the device obtained successfully." : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	    tapEnv.waitTill(BroadBandTestConstants.FOUR_MINUTES);

	    /**
	     * STEP : Verify if connected client is able to access the internet by pinging the site : 'www.google.com'
	     * <li>EXPECTED: Connected client should be able to access the site : 'www.google.com'
	     */
	    // boolean variable to store the status
	    status = false;
	    // Test step number
	    stepNumber = stepNumbers[1];
	    // Error message
	    errorMessage = "Connected client not able to access the site : 'www.google.com'";
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify if connected client is able to access the internet by pinging the site : 'www.google.com'");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute command WINDOWS : ping www.google.com -n 1 | grep 'Reply from' or LINUX : curl -4 -f --interface <interfaceName> www.google.com | grep '200 OK' on the connected client");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Connected client should be able to access the site : 'www.google.com'");
	    LOGGER.info("#######################################################################################");
	    status = ConnectedNattedClientsUtils.verifyNetworkConnection(connectedClientSettop, tapEnv, false);
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "Connected client able to access the site : 'www.google.com' successfully."
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	}
	return connectedClientSettop;
    }

    /**
     * Common step to apply and verify Dnsoverride by adding DNS mapping table
     * 
     * @param device
     * @param xdnsOverRideLevelIpv4
     * @param xdnsOverRideLevelIpv6
     * @param clientMacAddress
     * @param testCaseId
     * @param stepNumber
     * 
     * @refactor yamini.s
     */

    private void applyDnsoverrideByAddingDNSMappingTable(Dut device, String xdnsOverRideLevelIpv4,
	    String xdnsOverRideLevelIpv6, String clientMacAddress, String testCaseId, String stepNumber) {

	/**
	 * STEP : Verify the presence of the dnsmasq_servers.conf file in the device and Add DNS mapping table
	 * 
	 * <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table should be added successfully.
	 */
	// boolean variable to store the status
	boolean status = false;
	// Error message
	String errorMessage = "Attempt to apply dnsoverride through DNS mapping table failed.";
	LOGGER.info("#######################################################################################");
	LOGGER.info("STEP " + stepNumber
		+ ": DESCRIPTION : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table with values MacAddress : "
		+ clientMacAddress + ", DnsIPv4 : " + xdnsOverRideLevelIpv4 + ", DnsIPv6 : " + xdnsOverRideLevelIpv6
		+ ".");
	LOGGER.info("STEP " + stepNumber
		+ ": ACTION : Add DNS mapping table using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DNSMappingTable.'");
	LOGGER.info(
		"EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table should be added successfully.");
	LOGGER.info("#######################################################################################");
	status = BroadBandConnectedClientUtils.applyXdnsOverRideAddingDNSMapTbl(tapEnv, device, clientMacAddress,
		xdnsOverRideLevelIpv4, xdnsOverRideLevelIpv6);
	LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		+ (status ? "Applied dnsoverride by adding DNS mapping table successfully." : errorMessage));
	tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
    }

    /**
     * Common step to verify network connectivity in the connected client by accessing "www.google.com" after
     * blocking/allowing the connected client through DNS mapping table.
     * 
     * @param device
     * @param connectedClientSettop
     * @param siteAddressToPing
     * @param testCaseId
     * @param stepNumber
     * @param allowSiteAccess
     *            boolean variable representing whether the connected client should be blocked or allowed from accessing
     *            the site
     * 
     * @refactor yamini.s
     */

    private void verifySiteAccessInConnectClient(Dut device, Dut connectedClientSettop, String siteAddressToPing,
	    String expectedSiteAddress, String testCaseId, String stepNumber, boolean allowSiteAccess) {

	/**
	 * STEP : Verify xdns override at client mac Level by validating the domain name of the site passed after
	 * blocking/allowing the connected client through DNS mapping table.
	 * <li>EXPECTED: The corresponding site should be allowed/blocked as expected.
	 */

	// boolean variable to store the status
	boolean status = false;
	// Error message
	String errorMessage = allowSiteAccess
		? "The response doesn't have '" + expectedSiteAddress + "' as domain name for the site : '"
			+ siteAddressToPing
		: "The response doesn't have '" + expectedSiteAddress + "' as domain name for the site : '"
			+ siteAddressToPing + "', Even after blocking the site through DNS table.";
	// String to store the step description
	String stepDescription = allowSiteAccess
		? "Verify xdns override at client mac Level by validating the domain name of the site : "
			+ siteAddressToPing + ", After Factory resetting the router"
		: "Verify xdns override at client mac Level by validating the domain name of the site : "
			+ siteAddressToPing + ", After applying DNS override at client mac level";
	LOGGER.info("#######################################################################################");
	LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : " + stepDescription);
	LOGGER.info("STEP " + stepNumber + ": ACTION : Execute command 'nslookup " + siteAddressToPing
		+ " <IPv4>' on the connected client");
	LOGGER.info("STEP " + stepNumber + ": EXPECTED : The response should have '" + expectedSiteAddress
		+ "' as domain name for the site : '" + siteAddressToPing + "'");
	LOGGER.info("#######################################################################################");
	long startTime = System.currentTimeMillis();
	// Polling for 120 sec
	do {
	    status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
		    connectedClientSettop, siteAddressToPing,
		    BroadBandTestConstants.STRING_DEFAULT_GLOBAL_DNS_IPV4_VALUE, expectedSiteAddress);
	    if (status) {
		break;
	    }
	    // Waiting for 30 sec
	    LOGGER.info("Going to wait for 30sec and retry.");
	    tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	} while (System.currentTimeMillis() < (startTime + BroadBandTestConstants.TWO_MINUTES) && (!status));

	LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		+ (status
			? "The response has '" + expectedSiteAddress + "' as domain name for the site : '"
				+ siteAddressToPing + "' as expected."
			: errorMessage));
	tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
    }

    /**
     * Common step to execute Factory reset on the router device
     * 
     * @param device
     *            The device on which the factory reset has to be performed
     * @param testCaseId
     * 
     * @param stepNumber
     * 
     * @refactor yamini.s
     */

    private void executeFactoryResetOnTheDevice(Dut device, String testCaseId, String stepNumber) {
	/**
	 * STEP : Factory Reset the device and verify if device comes up
	 * <li>EXPECTED: Device should come up after Factory reset
	 */
	// boolean variable to store the status
	boolean status = false;
	LOGGER.info("#######################################################################################");
	LOGGER.info(
		"STEP :  " + stepNumber + " : DESCRIPTION : Factory Reset the device and verify if device comes up");
	LOGGER.info("STEP :  " + stepNumber
		+ " : ACTION : Factory Reset the device using webpa param Device.X_CISCO_COM_DeviceControl.FactoryReset");
	LOGGER.info("STEP :  " + stepNumber + " : EXPECTED: Device should come up after Factory reset");
	LOGGER.info("#######################################################################################");
	// Error message
	String errorMessage = "Failed to Tigger Factory Reset/Device didn't come up after Factory Reset.";
	status = BroadBandCommonUtils.performFactoryResetWebPaByPassingTriggerTime(tapEnv, device,
		BroadBandTestConstants.EIGHT_MINUTE_IN_MILLIS);
	if (status) {
	    LOGGER.info("STEP :  " + stepNumber + " ACTUAL : Device came up after Factory reset successfully.");
	} else {
	    LOGGER.error("STEP :  " + stepNumber + " ACTUAL : " + errorMessage);
	}
	tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
    }

    /**
     * Common step to set default values to Global DNS IPv4 and Global DNS IPv6
     * 
     * @param device
     * @param testCaseId
     * @param stepNumber
     * 
     * @refactor yamini.s
     */

    private void setAndVerifyValuesToGblDNSIPv4IPv6(Dut device, String testCaseId, String stepNumber) {
	/**
	 * STEP : Set and verify the Global DNS IPv4 value to '<IPv4>' and Global DNS IPv6 value to '<IPv6>'
	 * <li>EXPECTED: Global DNS IPv4 and IPv6 values should be set to '<IPv4>' and '<IPv6>' respectively.
	 */
	// boolean variable to store the status
	boolean status = false;
	// Error message
	String errorMessage = null;
	LOGGER.info("#######################################################################################");
	LOGGER.info("STEP " + stepNumber
		+ ": DESCRIPTION : Set and verify the Global DNS IPv4 value to '<IPv4>' and Global DNS IPv6 value to '<IPv6>'");
	LOGGER.info("STEP " + stepNumber
		+ ": ACTION 1 : Set the Global DNS IPv4 value to '<IPv4>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
	LOGGER.info("STEP " + stepNumber
		+ ": ACTION 2 : Set the Global DNS IPv6 value to '<IPv6>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'");
	LOGGER.info("STEP " + stepNumber
		+ ": EXPECTED : Global DNS IPv4 and IPv6 values should be set to '<IPv4>' and '<IPv6>' respectively.");
	LOGGER.info("#######################################################################################");
	// Error message
	errorMessage = "Failed to set Global DNS IPv4 value to '<IPv4>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'";
	if (BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV4, WebPaDataTypes.STRING.getValue(),
		BroadBandTestConstants.STRING_DEFAULT_GLOBAL_DNS_IPV4_VALUE, BroadBandTestConstants.THREE_MINUTES,
		BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)) {
	    // Error message
	    errorMessage = "Failed to set Global DNS IPv6 value to '<IPv6>' using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv6'";
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV6, WebPaDataTypes.STRING.getValue(),
		    BroadBandTestConstants.STRING_DEFAULT_GLOBAL_DNS_IPV6_VALUE, BroadBandTestConstants.THREE_MINUTES,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	}
	LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		+ (status ? "Global DNS IPv4 and IPv6 values sucessfully set to '<IPv4>' and '<IPv6>' respectively."
			: errorMessage));
	tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

    }

    /**
     * Common step to verify that the router device is in CaptivePortal Mode after factory reset, reactivate and connect
     * the client device to the SSID.
     * 
     * @param device
     * @param connectedClientDevice
     * @param testCaseId
     * @param stepNumber
     * 
     * @refactor yamini.s
     */

    private void verifyCaptivePrtlModeToReactivateRtrAndConnctClient(Dut device, Dut connectedClientSettop,
	    String testCaseId, String stepNumber) {
	/**
	 * STEP : Verify Captive portal status of the router after factory reset and connect the client device to the
	 * SSID after reactivating the router.
	 * <li>EXPECTED: Device should be in Captive portal mode after factory reset and the connected clients should
	 * able to access Network after reactivating the router
	 */
	// boolean variable to store the status
	boolean status = false;
	String ssidName = null;
	// Error message
	String errorMessage = null;
	String passPhraseName = null;
	String defaultPartner = null;
	LOGGER.info("#######################################################################################");

	if (!DeviceModeHandler.isDSLDevice(device)) {
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify Captive portal status of the router after factory reset and connect the client device to the SSID after reactivating the router.");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : ACTION : Get Captive portal status using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_CaptivePortalEnable and re-activate the router.");
	    LOGGER.info(
		    "EXPECTED: Device should be in Captive portal mode after factory reset and the client device should be able to connect to the SSID after reactivating the router.");
	} else {
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : After factory reset connect the client device to the SSID after reactivating the router.");
	    LOGGER.info("STEP :  " + stepNumber + " : ACTION : Re-activate the router.");
	    LOGGER.info(
		    "EXPECTED: After factory reset the client device should be able to connect to the SSID after reactivating the router.");
	}
	LOGGER.info("#######################################################################################");

	boolean captivePortalStatus = true;
	try {
	    if (!DeviceModeHandler.isDSLDevice(device)) {
		// Error message
		errorMessage = "Device is not in Captive portal after factory resetting.";
		captivePortalStatus = BroadBandWiFiUtils.verifyCaptivePortalModeUsingWebPaCommand(tapEnv, device);
	    }
	    if (captivePortalStatus) {
		if (!DeviceModeHandler.isDSLDevice(device)) {
		    LOGGER.info("Captive portal is enabled in the router after factory reset.");
		}
		LOGGER.info("Going to Reactivate the router.");
		try {
		    // 'reactivateDeviceUsingWebPa' method throws TestException if
		    // the re-activation process fails.
		    BroadBandWiFiUtils.reactivateDeviceUsingWebpaOrSnmp(tapEnv, device);
		    LOGGER.info("Successfully Reactivated the router.");
		    String wifiCapability = ((Device) connectedClientSettop).getConnectedDeviceInfo()
			    .getWifiCapability();
		    ssidName = (CommonMethods.isNotNull(wifiCapability) && (wifiCapability
			    .equalsIgnoreCase(BroadBandConnectedClientTestConstants.STRING_WIFI_CAPABILITY_2_4GHZ_ONLY)
			    || wifiCapability.equalsIgnoreCase(
				    BroadBandConnectedClientTestConstants.STRING_WIFI_CAPABILITY_DUAL_BAND)))
					    ? BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(
						    device, tapEnv, WiFiFrequencyBand.WIFI_BAND_2_GHZ)
					    : BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(
						    device, tapEnv, WiFiFrequencyBand.WIFI_BAND_5_GHZ);

		    passPhraseName = (CommonMethods.isNotNull(wifiCapability) && (wifiCapability
			    .equalsIgnoreCase(BroadBandConnectedClientTestConstants.STRING_WIFI_CAPABILITY_2_4GHZ_ONLY)
			    || wifiCapability.equalsIgnoreCase(
				    BroadBandConnectedClientTestConstants.STRING_WIFI_CAPABILITY_DUAL_BAND)))
					    ? BroadBandConnectedClientUtils
						    .getSsidPassphraseFromGatewayUsingWebPaOrDmcli(device, tapEnv,
							    WiFiFrequencyBand.WIFI_BAND_2_GHZ)
					    : BroadBandConnectedClientUtils
						    .getSsidPassphraseFromGatewayUsingWebPaOrDmcli(device, tapEnv,
							    WiFiFrequencyBand.WIFI_BAND_5_GHZ);
		    // Error message
		    errorMessage = "Unable to obtain SSID/passphrase from the router";
		    if (CommonMethods.isNotNull(ssidName) && CommonMethods.isNotNull(passPhraseName)) {
			LOGGER.info("Obtained SSID : " + ssidName + " and PassPhrase : " + passPhraseName
				+ ". Going to connect the client with the router.");
			status = ConnectedNattedClientsUtils.connectToSSID(connectedClientSettop, tapEnv, ssidName,
				passPhraseName);
			errorMessage = "Unable to connect the client to the router after reactivating/Factory Resetting the router.";
		    }
		} catch (TestException e) {
		    errorMessage = e.getMessage();
		}
	    }
	} catch (Exception exe) {
	    errorMessage = "Following Exception occurred while querying the Captive portal status using WebPA/Dmcli command  -> "
		    + exe.getMessage();
	}
	if (!DeviceModeHandler.isDSLDevice(device)) {
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: " + (status
		    ? "Router is in Captive portal mode after factory reset. The client device is connected to the SSID successfully after reactivating the router."
		    : errorMessage));
	} else {
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "The client device is connected to the SSID successfully after reactivating the router."
			    : errorMessage));
	}
	tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
    }

    /**
     * Common step to apply and verify different cases of Invalid values for MAC and DNS IP are not accepted for
     * dnsoverride
     * 
     * @param device
     * @param connectedClientSettop
     * @param siteAddressToPing
     * @param xdnsOverRideLevelIpv4
     * @param clientMacAddress
     * @param xdnsOverRideLevelIpv6
     * @param testCaseId
     * @param stepNumbers
     * 
     * @refactor yamini.s
     */

    private void applyInvalDnsoverrideValsAndVerifySiteNtBlckd(Dut device, Dut connectedClientSettop,
	    String siteAddressToPing, String expectedsiteAddress, String xdnsOverRideLevelIpv4, String clientMacAddress,
	    String xdnsOverRideLevelIpv6, String testCaseId, String[] stepNumbers) {

	// Test step number
	String stepNumber = null;
	// boolean variable to store the status
	boolean status = false;
	// Error message
	String errorMessage = null;
	if (stepNumbers.length == 2) {
	    /**
	     * STEP : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table with
	     * given values
	     * 
	     * <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table should be added successfully
	     * with the given Invalid values of MacAddress/DnsIPv4.
	     */
	    // Test step number
	    stepNumber = stepNumbers[0];
	    // boolean variable to store the status
	    status = false;
	    errorMessage = "Attempt to apply dnsoverride with invalid macaddress/dnsserver value to DNS mapping table failed.";
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table with values MacAddress : "
		    + clientMacAddress + ", DnsIPv4 : " + xdnsOverRideLevelIpv4 + ", DnsIPv6 : " + xdnsOverRideLevelIpv6
		    + ".");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Add DNS mapping table using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DNSMappingTable.'");
	    LOGGER.info(
		    "EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table should be added successfully with the given Invalid values of MacAddress/DnsIPv4.");
	    LOGGER.info("#######################################################################################");
	    status = BroadBandConnectedClientUtils.applyXdnsOverRideAddingDNSMapTbl(tapEnv, device, clientMacAddress,
		    xdnsOverRideLevelIpv4, xdnsOverRideLevelIpv6);
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "Added DNS Mapping table with invalid macaddress/dnsserver value successfully."
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	    /**
	     * STEP : Verify xdns override at client mac Level by validating the domain name of the site passed after
	     * applying dns override with Invalid macaddress/dnsserver values
	     * <li>EXPECTED: The response should have the site name as domain name
	     */
	    // Test step number
	    stepNumber = stepNumbers[1];
	    // boolean variable to store the status
	    status = false;
	    // Error message
	    errorMessage = "The response doesn't have '" + expectedsiteAddress + "' as domain name for the site : '"
		    + siteAddressToPing + "'.";
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify xdns override at client mac Level by validating the domain name of the site : "
		    + siteAddressToPing + ", after applying dns override with Invalid macaddress/dnsserver values");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute command 'nslookup " + siteAddressToPing
		    + " <IPv4>' on the connected client");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : The response should have '" + siteAddressToPing
		    + "' as domain name for the site : '" + siteAddressToPing + "'");
	    LOGGER.info("#######################################################################################");
	    status = BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
		    connectedClientSettop, siteAddressToPing,
		    BroadBandTestConstants.STRING_DEFAULT_GLOBAL_DNS_IPV4_VALUE, expectedsiteAddress);
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status
			    ? "The response has '" + siteAddressToPing + "' as domain name for the site : '"
				    + siteAddressToPing + "', as expected."
			    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);

	}
    }

    /**
     * This Test case is written to apply and verify Xdns override at gateway level.
     * 
     * STEP 1 : Enable and verify the XDNS feature using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS
     * <li>EXPECTED: XDNS feature should be enabled using webpa param 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'
     *
     * STEP 2 : Obtain a working Connected client associated with the device
     * <li>EXPECTED: Connected client should be obtained successfully
     *
     * STEP 3 : Verify if connected client is able to access the internet by pinging the site : 'www.google.com'
     * <li>EXPECTED: Connected client should be able to access the site : 'www.google.com'
     *
     * STEP 4 : Set and verify the Global DNS IPv4 value using webpa param
     * 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'
     * <li>EXPECTED : Global DNS IPv4 value should be set using webpa param
     * 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'
     *
     * STEP 5 : Verify xdns override at gateway Level by validating the domain name of the site : www.seriesw.net on the
     * connected client
     * <li>EXPECTED: DNS request for server should fail with 'timed out' error
     * 
     * @param Dut
     *            The device to be used.
     * @author Susheela C
     * @Refactor Alan_Bivera
     * 
     */

    @Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    TestGroup.XDNS })
    @TestDetails(testUID = "TC-RDKB-XDNS-1001")
    public void applyAndVerifyXdnsOverrideAtGatewayLevel(Dut device) {
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-XDNS-1001");
	LOGGER.info("TEST DESCRIPTION: To apply and verify Xdns override at gateway level");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info(
		"STEP 1 : Enable and verify the XDNS feature using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS");
	LOGGER.info("STEP 2 : Obtain a working Connected client associated with the device");
	LOGGER.info(
		"STEP 3 : Verify if connected client is able to access the internet by pinging the site : 'www.google.com'");
	LOGGER.info(
		"STEP 4 : Set and verify the Global DNS IPv4 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
	LOGGER.info(
		"STEP 5 : Verify xdns override at gateway Level by validating the domain name of the site : www.seriesw.net on the connected client");
	LOGGER.info("#######################################################################################");
	// Holds the test case ID
	String testCaseId = "TC-RDKB-XDNS-101";
	// boolean variable to store the status
	boolean status = false;
	// Test step number
	String stepNumber = null;
	// Error message
	String errorMessage = null;
	try {

	    executePreConditions(device, testCaseId, true);
	    // Test step number
	    stepNumber = "s1";
	    // boolean variable to store the status
	    status = false;
	    setAndVerifyXdnsFeature(device, testCaseId, stepNumber, true);

	    // Test step number
	    stepNumber = "s2";
	    // boolean variable to store the status
	    status = false;
	    Dut connectedClientSettop = verifyConnectedClientAndInternetAccess(device, testCaseId,
		    new String[] { "s2", "s3" });

	    /**
	     * STEP 4 : Set and verify the Global DNS IPv4 value using webpa param
	     * 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'
	     * <li>EXPECTED : Global DNS IPv4 value should be set using webpa param
	     * 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'
	     */
	    // step number
	    stepNumber = "s4";
	    // boolean variable to store the status
	    status = false;
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Set and verify the Global DNS IPv4 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : ACTION : Set the Global DNS IPv4 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : EXPECTED : Global DNS IPv4 value should be set using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'");
	    LOGGER.info("#######################################################################################");
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_UPDATE_GLOBAL_XDNS_IPV4, WebPaDataTypes.STRING.getValue(),
		    BroadBandTestConstants.STRING_DNS_GATEWAY_LEVEL_OVERRIDE_IPV4_VALUE,
		    BroadBandTestConstants.THREE_MINUTES, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    // Error message
	    errorMessage = "Failed to set Global DNS IPv4 value using webpa param 'Device.X_RDKCENTRAL-COM_XDNS.DefaultDeviceDnsIPv4'";
	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "Global DNS IPv4 value sucessfully set" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	    /**
	     * STEP 5 : Verify xdns override at gateway Level by validating the domain name of the site :
	     * www.seriesw.net on the connected client
	     * <li>EXPECTED: DNS request for server should fail with 'timed out' error
	     */
	    stepNumber = "s5";
	    // boolean variable to store the status
	    status = false;
	    // string to store site address for level one site from properties
	    // file
	    String addressForLevelOneSite = AutomaticsTapApi
		    .getSTBPropsValue(BroadBandTestConstants.PROP_KEY_HOST_ADDRESS_FOR_LEVEL_ONE_SITE);
	    // Error message
	    errorMessage = "The response has '" + addressForLevelOneSite + "' as domain name for the site : "
		    + addressForLevelOneSite + "', Even after blocking the client at gateway level.";
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify xdns override at gateway Level by validating the domain name of the site : "
		    + addressForLevelOneSite + ", after blocking the client at gateway level");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute command 'nslookup " + addressForLevelOneSite
		    + " on the connected client");
	    LOGGER.info(
		    "STEP " + stepNumber + ": EXPECTED : DNS request for server should fail with 'timed out' error");
	    LOGGER.info("#######################################################################################");
	    // Instance to store the client settop
	    Device ecastSettop = (Device) connectedClientSettop;
	    // String to store ssh command
	    String sshCommand = BroadBandCommonUtils.concatStringUsingStringBuffer("nslookup",
		    AutomaticsConstants.SPACE, addressForLevelOneSite, AutomaticsConstants.SPACE,
		    BroadBandTestConstants.STRING_DEFAULT_GLOBAL_DNS_IPV4_VALUE);
	    long startTime = System.currentTimeMillis();
	    // Polling for 120 sec
	    do {
		// String to store response obtained from connected client
		String response = tapEnv.executeCommandOnOneIPClients(ecastSettop, sshCommand);
		status = (CommonMethods.patternMatcher(response,
			BroadBandTestConstants.STRING_DNS_REQUEST_TIMED_OUT_ERROR_MESSAGE)
			|| !BroadBandConnectedClientUtils.verifyDNSConfigInCnctdClientPassingHostAddrAndDnsSrvr(tapEnv,
				connectedClientSettop, addressForLevelOneSite,
				BroadBandTestConstants.STRING_DEFAULT_GLOBAL_DNS_IPV4_VALUE, addressForLevelOneSite));
		if (status) {
		    break;
		}
		// Waiting for 30 sec
		LOGGER.info("Going to wait for 30sec and retry.");
		tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    } while (System.currentTimeMillis() < (startTime + BroadBandTestConstants.TWO_MINUTES) && (!status));

	    LOGGER.info("STEP " + stepNumber + " - ACTUAL: "
		    + (status ? "DNS request for server failed with 'timed out' error as expected." : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	} catch (TestException exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("TC-RDKB-XDNS-1001 : Execution error occured due to exception --> " + errorMessage);
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	} finally {
	    BroadBandPostConditionUtils.executePostConditionForXdns(device, tapEnv, false);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	    LOGGER.info("TEST CASE END : TC-RDKB-XDNS-1001");
	}
    }

    /**
     * This Test case is written as a part of automation testing to apply and Verify XDNS override of Primary Level 1 ,2
     * and 3 IPv4 and IPv6 dns server IPs for connected client.
     * 
     * STEP 1 : Enable and verify the XDNS feature using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS
     * <li>EXPECTED: XDNS feature should be enabled/disabled using webpa param
     * 'Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS'
     *
     * STEP 2 : Obtain a working Connected client associated with the device
     * <li>EXPECTED: Connected client should be obtained successfully
     *
     * STEP 3 : Verify if connected client is able to access the internet by pinging the site : 'www.google.com'
     * <li>EXPECTED: Connected client should be able to access the site : 'www.google.com'
     *
     * STEP 4 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table with values
     * MacAddress : Wi-Fi MacAddress of the connected client , DnsIPv4 , DnsIPv6 
     * <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table should be added successfully.
     *
     * STEP 5 : Verify xdns override at client mac Level by validating the domain name of the site : www.seriesw.net on
     * the connected client
     * <li>EXPECTED: The response should have 'xdns-low-gslb.gslb2.comcast.com' as domain name for the site :
     * 'www.seriesw.net'
     *
     * STEP 6 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table with values
     * MacAddress : Wi-Fi MacAddress of the connected client , DnsIPv4 , DnsIPv6
     * <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table should be added successfully.
     *
     * STEP 7 : Verify xdns override at client mac Level by validating the domain name of the site :
     * www.plannedparenthood.org on the connected client
     * <li>EXPECTED: The response should have 'xdns-medium-gslb.gslb2.comcast.com' as domain name for the site :
     * 'www.plannedparenthood.org'
     *
     * STEP 8 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table with values
     * MacAddress : Wi-Fi MacAddress of the connected client
     * <li>EXPECTED: dnsmasq_servers.conf file should exist and DNS mapping table should be added successfully.
     *
     * STEP 9 : Verify xdns override at client mac Level by validating the domain name of the site : www.ar15.com on the
     * connected client
     * <li>EXPECTED: The response should have 'xdns-high-gslb.gslb2.comcast.com' as domain name for the site :
     * 'www.ar15.com'
     *
     * STEP 10 : Factory Reset the device and verify if device comes up
     * <li>EXPECTED: Device should come up after Factory reset
     *
     * STEP 11 : Set and verify the Global DNS IPv4 value and Global DNS IPv6 value
     * <li>EXPECTED: Global DNS IPv4 and IPv6 values should be set.
     * 
     * STEP 12 : Verify Captive portal status of the router after factory reset and connect the client settop to the
     * SSID after reactivating the router.
     * <li>EXPECTED: Device should be in Captive portal mode after factory reset and the connected clients should able
     * to access Network after reactivating the router
     *
     * STEP 13 : Verify connected client is able to access the site : www.seriesw.net, After Factory resetting the
     * router
     * <li>EXPECTED: The response should have 'www.seriesw.net' as the domain name for the site : www.seriesw.net
     *
     * STEP 14 : Verify connected client is able to access the site : www.plannedparenthood.org, After Factory resetting
     * the router
     * <li>EXPECTED: The response should have 'www.plannedparenthood.org' as the domain name for the site :
     * www.plannedparenthood.org
     *
     * STEP 15 : Verify connected client is able to access the site : www.ar15.com, After Factory resetting the router
     * <li>EXPECTED: The response should have 'www.ar15.com' as the domain name for the site : www.ar15.com
     * 
     * @param settop
     *            The settop to be used.
     * @author Susheela C
     * @refactor Alan_Bivera
     */

    @Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.XDNS })
    @TestDetails(testUID = "TC-RDKB-XDNS-1002")
    public void applyAndVerifyXdnsOverrideAtClientMacLevelPrimaryUsingIPv4AndIPv6(Dut device) {
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-XDNS-1002");
	LOGGER.info(
		"TEST DESCRIPTION: To apply and Verify XDNS override of Primary Level 1 ,2 and 3 IPv4 and IPv6 dns server IPs for connected client");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info(
		"STEP 1 : Enable and verify the XDNS feature using webpa param Device.DeviceInfo.X_RDKCENTRAL-COM_EnableXDNS");

	LOGGER.info("STEP 2 : Obtain a working Connected client associated with the device");
	LOGGER.info(
		"STEP 3 : Verify if connected client is able to access the internet by pinging the site : 'www.google.com'");
	LOGGER.info(
		"STEP 4 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table with values MacAddress : Wi-Fi MacAddress of the connected client , DnsIPv4 , DnsIPv6");
	LOGGER.info(
		"STEP 5 : Verify xdns override at client mac Level by validating the domain name of the site : www.seriesw.net on the connected client");
	LOGGER.info(
		"STEP 6 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table with values MacAddress : Wi-Fi MacAddress ofthe connected client ");
	LOGGER.info(
		"STEP 7 : Verify xdns override at client mac Level by validating the domain name of the site : www.plannedparenthood.org on the connected client");
	LOGGER.info(
		"STEP 8 : Verify the presence of the dnsmasq_servers.conf file in the device and add DNS mapping table with values MacAddress : Wi-Fi MacAddress of the connected client");
	LOGGER.info(
		"STEP 9 : Verify xdns override at client mac Level by validating the domain name of the site : www.ar15.com on the connected client");
	LOGGER.info("STEP 10 : Factory Reset the device and verify if device comes up");
	LOGGER.info(
		"STEP 11 : Set and verify the Global DNS IPv4 value ");
	LOGGER.info(
		"STEP 12 : Verify Captive portal status of the router after factory reset and connect the client settop to the SSID after reactivating the router.");
	LOGGER.info(
		"STEP 13 : Verify xdns override at client mac Level by validating the domain name of the site : www.seriesw.net, After Factory resetting the router");
	LOGGER.info(
		"STEP 14 : Verify xdns override at client mac Level by validating the domain name of the site : www.plannedparenthood.org, After Factory resetting the router");
	LOGGER.info(
		"STEP 15 : Verify xdns override at client mac Level by validating the domain name of the site : www.ar15.com, After Factory resetting the router");
	LOGGER.info("#######################################################################################");
	// Holds the test case ID
	String testCaseId = "TC-RDKB-XDNS-102";
	// boolean variable to store the status
	boolean status = false;
	// Test step number
	String stepNumber = null;
	// Error message
	String errorMessage = null;
	try {
	    /**
	     * PRE-CONDITIONS
	     */
	    executePreConditions(device, testCaseId, true);
	    /**
	     * "s1"
	     */
	    // Test step number
	    stepNumber = "s1";
	    // boolean variable to store the status
	    status = false;
	    setAndVerifyXdnsFeature(device, testCaseId, stepNumber, true);
	    /**
	     * "s2", "s3"
	     */
	    // Test step number
	    stepNumber = "s2";
	    // boolean variable to store the status
	    status = false;
	    Dut connectedClientSettop = verifyConnectedClientAndInternetAccess(device, testCaseId,
		    new String[] { "s2", "s3" });

	    /**
	     * "s4"
	     */
	    // Test step number
	    stepNumber = "s4";
	    // boolean variable to store the status
	    status = false;
	    // Mac address of the connected client
	    String clientMacAddress = ((Device) connectedClientSettop).getConnectedDeviceInfo().getWifiMacAddress();
	    applyDnsoverrideByAddingDNSMappingTable(device,
		    BroadBandTestConstants.STRING_DNS_IPV4_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY,
		    BroadBandTestConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_ONE_PRIMARY, clientMacAddress,
		    testCaseId, "s4");
	    /**
	     * "s5"
	     */
	    // step number
	    stepNumber = "s5";
	    // boolean variable to store the status
	    status = false;
	    LOGGER.info("Waiting for 10 Seconds...");
	    tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
	    // string to store site address for level one site from properties
	    // file
	    String addressForLevelOneSite = AutomaticsTapApi
		    .getSTBPropsValue(BroadBandTestConstants.PROP_KEY_HOST_ADDRESS_FOR_LEVEL_ONE_SITE);
	    // string to store dns block site address for level one site from
	    // properties file
	    String dnsBlockAddressForLevelOneSite = AutomaticsTapApi
		    .getSTBPropsValue(BroadBandTestConstants.PROP_KEY_DNS_BLOCK_ADDRESS_FOR_LEVEL_ONE_SITE);
	    // Connectivity check for Level One site
	    verifySiteAccessInConnectClient(device, connectedClientSettop, addressForLevelOneSite,
		    dnsBlockAddressForLevelOneSite, testCaseId, "s5", false);
	    /**
	     * "s6"
	     */
	    // Test step number
	    stepNumber = "s6";
	    // boolean variable to store the status
	    status = false;
	    applyDnsoverrideByAddingDNSMappingTable(device,
		    BroadBandTestConstants.STRING_DNS_IPV4_VALUE_FOR_DNS_LEVEL_TWO_PRIMARY,
		    BroadBandTestConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_TWO_PRIMARY, clientMacAddress,
		    testCaseId, "s6");
	    /**
	     * "s7"
	     */
	    // Test step number
	    stepNumber = "s7";
	    // boolean variable to store the status
	    status = false;
	    LOGGER.info("Waiting for 10 Seconds...");
	    tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
	    // string to store site address for level two site from properties
	    // file
	    String addressForLevelTwoSite = AutomaticsTapApi
		    .getSTBPropsValue(BroadBandTestConstants.PROP_KEY_HOST_ADDRESS_FOR_LEVEL_TWO_SITE);
	    // string to store dns block site address for level two site from
	    // properties file
	    String dnsBlockAddressForLevelTwoSite = AutomaticsTapApi
		    .getSTBPropsValue(BroadBandTestConstants.PROP_KEY_DNS_BLOCK_ADDRESS_FOR_LEVEL_TWO_SITE);
	    // Connectivity check for Level Two site
	    verifySiteAccessInConnectClient(device, connectedClientSettop, addressForLevelTwoSite,
		    dnsBlockAddressForLevelTwoSite, testCaseId, "s7", false);
	    /**
	     * "s8"
	     */
	    // Test step number
	    stepNumber = "s8";
	    // boolean variable to store the status
	    status = false;
	    applyDnsoverrideByAddingDNSMappingTable(device,
		    BroadBandTestConstants.STRING_DNS_IPV4_VALUE_FOR_DNS_LEVEL_THREE_PRIMARY,
		    BroadBandTestConstants.STRING_DNS_IPV6_VALUE_FOR_DNS_LEVEL_THREE_PRIMARY, clientMacAddress,
		    testCaseId, "s8");
	    /**
	     * "s9"
	     */
	    // Test step number
	    stepNumber = "s9";
	    // boolean variable to store the status
	    status = false;
	    LOGGER.info("Waiting for 10 Seconds...");
	    tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
	    // string to store site address for level three site from properties
	    // file
	    String addressForLevelThreeSite = AutomaticsTapApi
		    .getSTBPropsValue(BroadBandTestConstants.PROP_KEY_HOST_ADDRESS_FOR_LEVEL_THREE_SITE);
	    // string to store dns block site address for level three site from
	    // properties file
	    String dnsBlockAddressForLevelThreeSite = AutomaticsTapApi
		    .getSTBPropsValue(BroadBandTestConstants.PROP_KEY_DNS_BLOCK_ADDRESS_FOR_LEVEL_THREE_SITE);
	    // Connectivity check for Level Three site
	    verifySiteAccessInConnectClient(device, connectedClientSettop, addressForLevelThreeSite,
		    dnsBlockAddressForLevelThreeSite, testCaseId, "s9", true);

	    /**
	     * "s10"
	     */
	    // Test step number
	    stepNumber = "s10";
	    // boolean variable to store the status
	    status = false;
	    executeFactoryResetOnTheDevice(device, testCaseId, stepNumber);

	    /**
	     * "s11"
	     */
	    // Test step number
	    stepNumber = "s11";
	    // boolean variable to store the status
	    status = false;
	    setAndVerifyValuesToGblDNSIPv4IPv6(device, testCaseId, "s11");

	    /**
	     * "s12"
	     */
	    // Test step number
	    stepNumber = "s12";
	    // boolean variable to store the status
	    status = false;
	    String defaultPartner = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_SYNDICATION_PARTNER_ID);
	    if (!defaultPartner.equalsIgnoreCase(BroadBandTestConstants.PARTNER_ID_COX)
		    || !defaultPartner.equalsIgnoreCase(BroadBandTestConstants.PARTNER_ID_SHAW)) {
		verifyCaptivePrtlModeToReactivateRtrAndConnctClient(device, connectedClientSettop, testCaseId,
			stepNumber);
	    } else {
		errorMessage = "This step is not applicable for Syndication Partner-COX and SHAW";
		LOGGER.info("STEP " + stepNumber + " - ACTUAL: " + errorMessage);
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	    /**
	     * "s13"
	     */
	    // Test step number
	    stepNumber = "s13";
	    // boolean variable to store the status
	    status = false;
	    // Connectivity check for Level One site
	    verifySiteAccessInConnectClient(device, connectedClientSettop, addressForLevelOneSite,
		    addressForLevelOneSite, testCaseId, "s13", true);
	    /**
	     * "s14"
	     */
	    // Test step number
	    stepNumber = "s14";
	    // boolean variable to store the status
	    status = false;
	    // Connectivity check for Level Two site
	    verifySiteAccessInConnectClient(device, connectedClientSettop, addressForLevelTwoSite,
		    addressForLevelTwoSite, testCaseId, "s14", true);
	    /**
	     * "s15"
	     */
	    // Test step number
	    stepNumber = "s15";
	    // boolean variable to store the status
	    status = false;
	    // Connectivity check for Level Three site
	    verifySiteAccessInConnectClient(device, connectedClientSettop, addressForLevelThreeSite,
		    addressForLevelThreeSite, testCaseId, "s15", true);

	} catch (TestException exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("TC-RDKB-XDNS-1002 : Execution error occured due to exception --> " + exception.getMessage());
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	} finally {
	    try {
		BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
		BroadBandPostConditionUtils.executePostConditionForXdns(device, tapEnv, true);
		LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	    } catch (Exception exception) {
		LOGGER.error(
			"TC-RDKB-XDNS-1002 : Execution error occurred while executing post conditions due to exception --> "
				+ exception.getMessage());
	    }
	}
    }

}
