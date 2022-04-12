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

package com.automatics.rdkb.tests.wifi;

import java.util.ArrayList;
import java.util.List;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaParameter;

public class BroadBandWifiWhixTest extends AutomaticsTestBase {

    /**
     * 
     * <li>PRECONDITION1: Connect a 2.4GHZ client to device</li>
     * <li>PRECONDITION2: Factory Reset the device</li>
     * <li>PRECONDITION3: Connect a 2.4GHZ client to device and Verify client is able to browse internet</li>
     * <li>1. Enable Open public hotspot using WebPA</li>
     * <li>2. Verify and set Interworking.Enable to true using WebPA</li>
     * <li>3. Verify InterworkingServiceCapability to be true for vAP5,6 using WebPA</li>
     * <li>4. Verify InterworkingServiceEnable to be true for vAP5,6 using WebPA</li>
     * <li>5. Verify InterworkingElement.Internet to be true for vAP5,6 using WebPA</li>
     * <li>6. Set IE Apply Settings to true using WebPA</li>
     * <li>7. Connect to 2.4 GHZ SSID after Interworking element changes applied</li>
     * <li>8. Verify if client is able to browse internet from 2.4GHZ private radio</li>
     * <li>POST-CONDITION: Set Interworking Parametersa to default values</li>
     * 
     * @author RamaTeja Meduri
     * @param device
     *            Dut instance
     * 
     * @Refactor Sruthi Santhosh
     */

    @Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    TestGroup.NEW_FEATURE, TestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-IE-1002")
    public void testToVerifyWifiIEConnectedClient(Dut device) {
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-IE-1002");
	LOGGER.info("TEST DESCRIPTION: Test to verify WiFi Interworking Information Element");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("PRE-CONDITION1: Connect a 2.4GHZ client to device");
	LOGGER.info("PRE-CONDITION2: Factory Reset the device");
	LOGGER.info("PRE-CONDITION3: Connect a 2.4GHZ client to device and Verify client is able to browse internet");
	LOGGER.info("1. Enable Open public hotspot using WebPA");
	LOGGER.info("2. Verify and set Interworking.Enable to true using WebPA");
	LOGGER.info("3. Verify InterworkingServiceCapability to be true for vAP5,6 using WebPA");
	LOGGER.info("4. Verify InterworkingServiceEnable to be true for vAP5,6 using WebPA");
	LOGGER.info("5. Verify InterworkingElement.Internet to be true for vAP5,6 using WebPA ");
	LOGGER.info("6. Set IE Apply Settings to true using WebPA");
	LOGGER.info("7. Connect  to 2.4 GHZ SSID after Interworking element changes applied");
	LOGGER.info("8. Verify if client is able to browse internet from 2.4GHZ private radio");
	LOGGER.info("PRE-CONDITION: Set Interworking Parametersa to default values");

	// Variable Declaration begins
	String testCaseId = null;
	String stepNum = null;
	String errorMessage = null;
	int stepNumber = BroadBandTestConstants.CONSTANT_1;
	boolean status = false;
	Dut clientDevice = null;
	String ssidName = null;
	String ssidPassPhrase = null;
	// Variable Declaration Ends

	testCaseId = "TC-RDKB-WIFI-IE-102";

	try {
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");

	    LOGGER.info("PRE-CONDITION 1: DESCRIPTION : Connect a 2.4GHz client to device ");
	    LOGGER.info(
		    "PRE-CONDITION 1: ACTION : Pick 2.4GHz client from the pool and connect to device with 2.4GHz SSID");
	    LOGGER.info("PRE-CONDITION 1: EXPECTED : Client should be successfully connected to 2.4GHz SSID");

	    clientDevice = BroadBandConnectedClientUtils
		    .get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
	    status = clientDevice != null;
	    if (status) {
		LOGGER.info("PRE-CONDITION 1: ACTUAL : Picked 2.4GHZ client and able to access internet");
	    } else {
		LOGGER.error(
			"PRE-CONDITION 1: ACTUAL : Unable to pick 2.4GHZ client from the client pool for this device");
		throw new TestException(errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");

	    BroadBandPreConditionUtils.executePreConditionToFactoryResetDevice(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_2);

	    LOGGER.info(
		    "PRE-CONDITION 3: DESCRIPTION : Connect 2.4GHZ client to device and Verify client is able to browse internet");
	    LOGGER.info(
		    "PRE-CONDITION 3: ACTION : Pick 2.4GHZ client from the pool and connect to device with 2.4GHZ SSID and check if client is able to browse internet");
	    LOGGER.info(
		    "PRE-CONDITION 3: EXPECTED : Client should be successfully connected to 2.4GHZ SSID and able to browse internet");

	    ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
		    WiFiFrequencyBand.WIFI_BAND_2_GHZ);
	    ssidPassPhrase = BroadBandConnectedClientUtils.getSsidPassphraseFromGatewayUsingWebPaOrDmcli(device, tapEnv,
		    WiFiFrequencyBand.WIFI_BAND_2_GHZ);
	    status = CommonMethods.isNotNull(ssidName) && CommonMethods.isNotNull(ssidPassPhrase);
	    if (status) {
		BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_CAPTIVE_PORTAL_ENABLE, BroadBandTestConstants.CONSTANT_3,
			BroadBandTestConstants.FALSE);
		BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_INFO_RDK_CENTRAL_CONFIGURE_WIFI,
			BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);
		long startTime = System.currentTimeMillis();
		do {
		    status = ConnectedNattedClientsUtils.connectToSSID(clientDevice, tapEnv, ssidName, ssidPassPhrase,
			    BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL, false);
		} while (!status
			&& (System.currentTimeMillis() - startTime) <= BroadBandTestConstants.TEN_MINUTE_IN_MILLIS
			&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.NINETY_SECOND_IN_MILLIS));
	    } else {
		errorMessage = "unable to fetch SSID and Password to connect 2.4 GHZ client";
	    }
	    if (status) {
		status = !BroadBandConnectedClientUtils
			.verifyInternetAccessUsingCurl(tapEnv, clientDevice, BroadBandTestConstants.URL_WIKIPEDIA)
			.isStatus();
		if (status) {
		    errorMessage = "Client is unable to access internet";
		}
	    } else {
		errorMessage = "Unable to connect to 2.4GHZ client after factory reset";
	    }

	    if (status) {
		LOGGER.info(
			"PRE-CONDITION 3: ACTUAL : 2.4 GHZ client connected to SSID and is able to browse internet after factory reset");
	    } else {
		LOGGER.error("PRE-CONDITION 3: ACTUAL : " + errorMessage);
		throw new TestException(errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");

	    stepNum = "s" + stepNumber;
	    status = false;

	    LOGGER.info("****************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: Enable Open public hotspot using WebPA");
	    LOGGER.info("STEP " + stepNumber + ": ACTION: Execute Command: "
		    + "tr181. Device.X_COMCAST-COM_GRE.Tunnel.1.DSCPMarkPolicy int 44"
		    + "tr181. Device.X_COMCAST-COM_GRE.Tunnel.1.PrimaryRemoteEndpoint string <BroadBandTestConstants.PRIMARY_REMOTE_ENDPOINT>"
		    + "tr181. Device.X_COMCAST-COM_GRE.Tunnel.1.SecondaryRemoteEndpoint string <BroadBandTestConstants.SECONDARY_REMOTE_ENDPOINT>"
		    + "tr181. Device.WiFi.SSID.5.SSID string <BroadBandTestConstants.PUBLIC_WIFI_SSID_2>"
		    + "tr181. Device.WiFi.SSID.6.SSID string <BroadBandTestConstants.PUBLIC_WIFI_SSID_5>"
		    + "tr181. Device.WiFi.SSID.5.Enable bool true" + "tr181. Device.WiFi.SSID.6.Enable bool true");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: Open Public Hotspot should be enabled");
	    LOGGER.info("****************************************************************");

	    errorMessage = "Open Public hotspot is not enabled";
	    long startTime = System.currentTimeMillis();
	    do {
		try {
		    status = BroadBandWebPaUtils.settingWebpaparametersForPublicWifi(device, tapEnv,
			    BroadBandTestConstants.DUAL_BAND);
		} catch (Exception e) {
		    LOGGER.error("Exception caught while enabling public wifi" + e.getMessage());
		}
	    } while (!status && (System.currentTimeMillis() - startTime) <= BroadBandTestConstants.TEN_MINUTE_IN_MILLIS
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.NINETY_SECOND_IN_MILLIS));
	    if (status) {
		tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Open public wifi hotspot is enabled");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    // ##################################################################################################//

	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    status = false;
	    LOGGER.info("****************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: Verify and set Interworking.Enable to true using WebPA");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute Command: tr181.Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.WiFi-Interworking.Enable bool true");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: Interworking.Enable should be set to true");
	    LOGGER.info("****************************************************************");
	    errorMessage = "unable to set  Interworking.Enable to true";
	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_IE_ENABLE, WebPaDataTypes.BOOLEAN.getValue(),
		    BroadBandTestConstants.TRUE, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Interworking.Enable is set to true");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    // ##################################################################################################//

	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    status = false;
	    ArrayList<String> setIEParams = new ArrayList<String>();
	    setIEParams.add(BroadBandTestConstants.ACCESS_POINT_MAPPING.get(BroadBandTestConstants.STRING_VALUE_FIVE));
	    setIEParams.add(BroadBandTestConstants.ACCESS_POINT_MAPPING.get(BroadBandTestConstants.STRING_VALUE_SIX));
	    LOGGER.info("****************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: Verify InterworkingServiceCapability to be true for vAP5,6 using WebPA");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute Command: tr181.Device.WiFi.AccessPoint.{i}.X_RDKCENTRAL-COM_InterworkingServiceCapability");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: InterworkingServiceCapability should be set to true for vAP 5,6");
	    LOGGER.info("****************************************************************");
	    errorMessage = "Unable to set InterworkingServiceCapability to true for vAP 5,6";
	    for (String wifiIndex : setIEParams) {
		status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_FOR_IE_SERVICE_CAPABILITY.replace(
				BroadBandTestConstants.TR181_NODE_REF, wifiIndex),
			BroadBandTestConstants.TRUE);
		if (!status) {
		    errorMessage = "Unable to set InterworkingServiceCapability to true for " + wifiIndex + " vAP";
		    break;
		}
	    }
	    if (status) {
		LOGGER.info(
			"STEP " + stepNumber + ": ACTUAL : InterworkingServiceCapability is set to true for vAP 5,6");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    // ##################################################################################################//

	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    status = false;
	    LOGGER.info("****************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": Verify InterworkingServiceEnable to be true for vAP5,6 using WebPA");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute Command: tr181..Device.WiFi.AccessPoint.{i}.X_RDKCENTRAL-COM_InterworkingServiceEnable");
	    LOGGER.info(
		    "STEP " + stepNumber + ": EXPECTED: InterworkingServiceEnable should be set to true for vAP 5,6");
	    LOGGER.info("****************************************************************");
	    errorMessage = "Unable to set InterworkingServiceEnable to true for vAP 5,6";
	    for (String wifiIndex : setIEParams) {
		status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_FOR_IE_SERVICE_ENABLE.replace(
				BroadBandTestConstants.TR181_NODE_REF, wifiIndex),
			WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE,
			BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
		if (!status) {
		    errorMessage = "Unable to set InterworkingServiceEnable to true for " + wifiIndex + " vAP";
		    break;
		}
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : InterworkingServiceEnable is set to true for vAP 5,6");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    // ##################################################################################################//

	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    status = false;
	    LOGGER.info("****************************************************************");
	    LOGGER.info(
		    "STEP " + stepNumber + ": Verify  InterworkingElement.Internet to be true for vAP5,6 using WebPA");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute Command: tr181..Device.WiFi.AccessPoint.{i}.X_RDKCENTRAL-COM_ InterworkingElement.Internet");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED:  InterworkingElement.Internet should be set to true for vAP 5,6");
	    LOGGER.info("****************************************************************");
	    errorMessage = "Unable to set  InterworkingElement.Internet to true for vAP 5,6";
	    for (String wifiIndex : setIEParams) {
		status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_FOR_IE_INTERNET.replace(
				BroadBandTestConstants.TR181_NODE_REF, wifiIndex),
			WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE,
			BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS);
		if (!status) {
		    errorMessage = "Unable to set  InterworkingElement.Internet to true for " + wifiIndex + " vAP";
		    break;
		}
	    }
	    if (status) {
		LOGGER.info(
			"STEP " + stepNumber + ": ACTUAL :  InterworkingElement.Internet is set to true for vAP 5,6");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    // ##################################################################################################//

	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    status = false;
	    LOGGER.info("****************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: Set Apply Settings to true on vAP's using WebPA");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute Command: tr181.Device.WiFi.AccessPoint.{i}.X_RDKCENTRAL-COM_InterworkingApplySettings bool true");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: InterworkingApplySettings should be set to true");
	    LOGGER.info("****************************************************************");
	    errorMessage = "Unable to set InterworkingApplySettings to true";
	    for (String wifiIndex : setIEParams) {
		status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_FOR_IE_SETTINGS
				.replace(BroadBandTestConstants.TR181_NODE_REF, wifiIndex),
			WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
		if (!status) {
		    errorMessage = "unable to apply seetings on " + wifiIndex + " vAP";
		    break;
		}
	    }
	    if (status) {
		tapEnv.waitTill(BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : InterworkingApplySettings are set to true");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    // ##################################################################################################//

	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": Connect to 2.4 GHZ SSID after Interworking element changes applied");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Connect client to 2.4 GHZ radio");
	    LOGGER.info(
		    "STEP " + stepNumber + ": EXPECTED :  Client should be successfully connected to 2.4 GHZ radio");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Client is unable to connect to 2.4 GHZ radio";
	    startTime = System.currentTimeMillis();
	    do {
		status = ConnectedNattedClientsUtils.connectToSSID(clientDevice, tapEnv, ssidName, ssidPassPhrase,
			BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL, false);
	    } while (!status && (System.currentTimeMillis() - startTime) <= BroadBandTestConstants.TEN_MINUTE_IN_MILLIS
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.NINETY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Client is getting connected to 2.4 GHZ radio");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    // ##################################################################################################/

	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP " + stepNumber + ": Verify if client is able to browse internet from 2.4GHZ private radio");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : client should browse internet from 2.4GHZ client");
	    LOGGER.info(
		    "STEP " + stepNumber + ": EXPECTED :  Client should be able to browse internet from 2.4GHZ client");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "2.4 GHZ client is unable to browse internet when IE is enabled on 5,6 vAP";

	    status = !BroadBandConnectedClientUtils
		    .verifyInternetAccessUsingCurl(tapEnv, clientDevice, BroadBandTestConstants.URL_WIKIPEDIA)
		    .isStatus();

	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Client is able to browse interent when IE is enabled");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    // ##################################################################################################/

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception Occurred while Verifying WiFi IE feature" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    status = false;
	    LOGGER.info("****************************************************************");
	    LOGGER.info("STEP POST-CONDITION: DESCRIPTION: Set Interworking Parameters to default values via Webpa");
	    LOGGER.info(
		    "STEP POST-CONDITION: ACTION: tr181.Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.WiFi-Interworking.Enable bool false"
			    + "tr181.Device.WiFi.AccessPoint.{i}.X_RDKCENTRAL-COM_InterworkingServiceEnable bool false"
			    + "tr181.Device.WiFi.AccessPoint.{i}.X_RDKCENTRAL-COM_InterworkingElement.AccessNetworkType uint 0"
			    + "tr181.Device.WiFi.AccessPoint.{i}.X_RDKCENTRAL-COM_InterworkingElement.Internet bool false");
	    LOGGER.info("STEP POST-CONDITION: EXPECTED: Interworking Parameters should be set to default");
	    LOGGER.info("****************************************************************");
	    errorMessage = "Failed to set Interworking parameters to default values";

	    ArrayList<String> setIEParams = new ArrayList<String>();
	    setIEParams.add(BroadBandTestConstants.ACCESS_POINT_MAPPING.get(BroadBandTestConstants.STRING_VALUE_FIVE));
	    setIEParams.add(BroadBandTestConstants.ACCESS_POINT_MAPPING.get(BroadBandTestConstants.STRING_VALUE_SIX));
	    List<WebPaParameter> webPaParameters = new ArrayList<WebPaParameter>();
	    for (String wifiIndex : setIEParams) {
		webPaParameters.add(BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
			BroadBandWebPaConstants.WEBPA_PARAM_FOR_IE_SERVICE_ENABLE
				.replace(BroadBandTestConstants.TR181_NODE_REF, wifiIndex),
			BroadBandTestConstants.FALSE, WebPaDataTypes.BOOLEAN.getValue()));
		webPaParameters.add(BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
			BroadBandWebPaConstants.WEBPA_PARAM_FOR_IE_INTERNET
				.replace(BroadBandTestConstants.TR181_NODE_REF, wifiIndex),
			BroadBandTestConstants.FALSE, WebPaDataTypes.BOOLEAN.getValue()));
		webPaParameters.add(BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
			BroadBandWebPaConstants.WEBPA_PARAM_FOR_IE_SETTINGS
				.replace(BroadBandTestConstants.TR181_NODE_REF, wifiIndex),
			BroadBandTestConstants.TRUE, WebPaDataTypes.BOOLEAN.getValue()));
	    }
	    status = BroadBandWebPaUtils.setMultipleParametersUsingWebPaOrDmcli(device, tapEnv, webPaParameters);

	    if (status) {
		webPaParameters.clear();
		webPaParameters.add(BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
			BroadBandWebPaConstants.WEBPA_PARAM_ENABLING_PUBLIC_WIFI, BroadBandTestConstants.FALSE,
			WebPaDataTypes.BOOLEAN.getValue()));
		webPaParameters.add(BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
			BroadBandWebPaConstants.WEBPA_PARAM_FOR_IE_ENABLE, BroadBandTestConstants.FALSE,
			WebPaDataTypes.BOOLEAN.getValue()));
		status = BroadBandWebPaUtils.setMultipleParametersUsingWebPaOrDmcli(device, tapEnv, webPaParameters);

		LOGGER.info("Status of set command is" + status);
	    }
	    if (status) {
		LOGGER.info("POST-CONDITION : ACTUAL : Interworking Element parameters are set to default values");
	    } else {
		LOGGER.error("POST-CONDITION : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-IE-1002");
	}
    }

}